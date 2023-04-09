#define LISTING__C

#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>

#include "listing.h"                /* My own definitions */

#include "util.h"
#include "assemble_globals.h"
#include "macros.h"
#include "rept_irpc.h"


/* GLOBAL VARIABLES */

char            title_string[32] = "";   /* .TITLE string (up to 31 characters) */
int             toc_shown = 0;           /* Flags that at least one .SBTTL was listed in the TOC */

static char    *list_page_fmt = "\n\n";  /* Format to use for the page throw */

int             list_page_top;      /* Are we at the top of a page? */

int             list_line_act;      /* Action to perform when listing the current line */

int             list_within_exp;    /* Flag whether the listing line is DIRECTLY within a macro/rept/irp/irpc expansion */

static char    *listline;           /* Source lines */

static char    *binline;            /* for octal expansion */

FILE           *lstfile = NULL;

int             list_pass_0 = 0;    /* Also list what happens during the first pass */

int             report_errcnt = 0;  /* Count the number of times report() has been called */

static int      errline = 0;        /* Set if current line has an error */

/* can_list returns TRUE if listing may happen for this line. */

static int can_list(
    void)
{
    int             ok = lstfile != NULL &&
                         (pass > 0 || list_pass_0);

    return ok;
}

/* dolist returns TRUE if listing is enabled. */

static int dolist(
    void)
{
    int             ok = can_list () &&
                         (list_level > 0 || errline);

    return ok;
}

/* list_source saves a text line for later listing by list_flush */

void list_source(
    STREAM *str,
    char *cp)
{
    if (can_list()) {
        int             len = strcspn(cp, "\n");

        /* Not an error yet */
        errline = 0;

        /* Save the line text away for later... */
        if (listline)
            free(listline);
        listline = memcheck(malloc(len + 1));
        memcpy(listline, cp, len);
        listline[len] = 0;

        /* Flag whether the listing line is within a macro/rept/irp/irpc expansion */
        list_within_exp = ((str->vtbl == &macro_stream_vtbl) ||
                           (str->vtbl ==  &rept_stream_vtbl) ||
                           (str->vtbl ==   &irp_stream_vtbl) ||
                           (str->vtbl ==  &irpc_stream_vtbl));

        if (!binline)
            binline = memcheck(malloc(sizeof(LSTFORMAT) + 16));

        sprintf(binline, "%*s%*d ", (int)SIZEOF_MEMBER(LSTFORMAT, flag), "",
                                    (int)SIZEOF_MEMBER(LSTFORMAT, line_number), str->line);
    }
}

/* list_throw_page starts a new page on the listing */
/* Note: Unlike MACRO-11 blank lines at the top of the page will be suppressed.
 *       For example, all EMPTY lines following a .INCLUDE will not be listed.
 *       Or, indeed, all of the first EMPTY lines in a file.  */

void list_throw_page(
    void)
{
    if (dolist() && !list_page_top) {
        fputs(list_page_fmt, lstfile);
    }
    list_page_top = 1;
}


/* trunc truncates a string by replacing all ' ' and '\t' at the end with '\0' */

static void trunc(
    char *string)
{
    int i;

    for (i = strlen(string) - 1; i >= 0; i--)
        if (string[i] == ' ' || string[i] == '\t')
            string[i] = '\0';
        else
            return;
}


/* list_oct2hex converts all octal numbers in a string to hex */
/* We can handle octal numbers with 1-6 digits. */

static void list_oct2hex(
    char *cp)
{
    char           *cpe;
    int             len;
    int             octval;
    char            oldche;
    const char      format[6][6] = { /* Digits   Octal   Hexadec */
                        "%1.1X",     /*      1       7         7 */
                        "%2.2X",     /*      2      77        3F */
                        "%3.2X",     /*      3     777       1FF */  /* But usually one byte (0x00-0xFF) */
                        "%4.3X",     /*      4    7777       FFF */
                        "%5.4X",     /*      5   77777      7FFF */
                        "%6.4X"      /*      6  177777      FFFF */
                    };

    for (;;) {
        while (!isdigit(*cp))
            if (*cp++ == '\0')
                return;

        octval = strtol(cp, &cpe, 8);
        len = cpe - cp;
        assert(len > 0 && len <= 6);

        oldche = *cpe;
        sprintf(cp, format[len-1], octval);
        *cpe = oldche;

        cp = cpe;
    }
}


/* list_process processes the listing line prior to outputting it */
/* This handles .[N]LIST SEQ,LOC,BIN,BEX,SRC,COM,HEX (except LOC without either SEQ or BIN). */
/* .NLIST COM is handled differently to MACRO-11 -- Lines starting with ';' or blank lines are suppressed */

static void list_process(
    void)
{
    int             binstart = 0;

    assert(isprint((unsigned char) binline[0]));

    if (LIST(TTM))
        padto(binline, offsetof(LSTFORMAT, words[1]));
    else
        padto(binline, offsetof(LSTFORMAT, source));

    trunc(listline);

    if (!errline) {

        /* Handle .NLIST SEQ,LOC,BIN,SRC */

        if (!(LIST(SEQ) || LIST(LOC) || LIST(BIN) || LIST(SRC)))
            return;  /* Completely suppress all [non-error] lines */

        /* Handle NLIST BEX [BIN] and .NLIST COM */

        if (binline[offsetof(LSTFORMAT, gap_after_seq) - 1] == ' ') {
            if (!LIST(BIN) || !LIST(BEX))
                return;  /* If no sequence number, suppress the BEX line */
        } else if (!LIST(COM)) {
            if (listline[0] == ';' || listline[0] == '\0')
            /*  if (binline[0] == ' ')  */
                    return;  /* Completely suppress LHS-comment lines and blank lines */
        }

        /* Handle .NLIST BIN[,LOC] */

        if (!LIST(BIN)) {
            binline[offsetof(LSTFORMAT, words)] = '\0';
            if (!LIST(LOC)) {
                binline[offsetof(LSTFORMAT, pc)] = '\0';
            }
        }

        /* Handle .NLIST SRC */

        if (!LIST(SRC)) {
            listline[0] = '\0';
            trunc(binline);
            if ( /* binline[0] == ' ' && */
                    strlen(binline) < offsetof(LSTFORMAT, pc))
                return;  /* Completely suppress source lines with only a sequence number */
        }

        /* Handle .NLIST ME,MEB */

        if (list_within_exp && !LIST(ME)) {
           if (!LIST(MEB))
               return;  /* Suppress all source lines within a macro/rept/irp/irpc expansion */

           /* Extension to MACRO-11 we only suppress ME lines which have no digits in the PC and WORDS fields */

           {
               int len = strlen(binline);
               int i;

               if (len < offsetof(LSTFORMAT, pc))
                   return;  /* Suppress non-data source lines within a macro/rept/irp/irpc expansion */

               for (i = offsetof(LSTFORMAT, pc); i < len; i++) {
                   if (isdigit((unsigned char) binline[i]))
                       break;
               }

               if (i >= len)
                   return;  /* Suppress non-data source lines within a macro/rept/irp/irpc expansion */
            }
        }

        /* Handle .NLIST SEQ[,LOC] */

        if (!LIST(SEQ)) {
            /* Note that we lose the 'flag' field this way */
            binstart = offsetof(LSTFORMAT, pc);
            if (!LIST(LOC))
                binstart = offsetof(LSTFORMAT, words);
        }
    }

    if (listline[0] == '\0')
        trunc(binline);

    /* Handle .LIST HEX */

    if (LIST(HEX))
        list_oct2hex(&binline[offsetof(LSTFORMAT, pc)]);

    /* Output the line */

    if (binline[0] != '\0')
        fputs(&binline[binstart], lstfile);
    if (listline[0] != '\0')
        fputs(listline, lstfile);
    fputc('\n', lstfile);
    list_page_top = 0;
}


/* list_flush produces a buffered list line. */

void list_flush(
    void)
{
    if (dolist()) {
        if (errline) {
            list_line_act &= ~LIST_SUPPRESS_LINE;  /* Error lines are never suppressed */
            if (pass == 0) {  /* TODO: Do this on both passes (?) */
                int             i;

                /* TODO: Implement error-letters like MACRO-11 (not trivial) */
                for (i = 0; i < errline; i++) {
                    if (binline[i] != ' ')
                        break;
                    binline[i] = '*';
                }
            }
        }

        if (!pass)
            list_line_act &= ~LIST_SUPPRESS_LINE;  /* Never suppress lines on pass 1 (for -yl1) */

        if (list_line_act & LIST_SUPPRESS_LINE) {
            list_line_act &= ~LIST_SUPPRESS_LINE;  /* Defer a LIST_PAGE_BEFORE */
        } else {
            if (list_line_act & LIST_PAGE_BEFORE) {
                list_line_act &= ~LIST_PAGE_BEFORE;
                list_throw_page();
            }
            list_process();
        }

        if (list_line_act & LIST_PAGE_AFTER) {
            list_line_act &= ~LIST_PAGE_AFTER;   /* Turn it into ... */
            list_line_act |=  LIST_PAGE_BEFORE;  /* ... a LIST_PAGE_BEFORE */
        }

        listline[0] = '\0';
        binline[0]  = '\0';
    }
}


/* list_fit checks to see if a word will fit in the current listing
   line.  If not, it flushes and prepares another line. */

static void list_fit(
    STREAM *str,
    unsigned addr)
{
    size_t          col1;
    size_t          col2 = offsetof(LSTFORMAT, pc);

    if (LIST(TTM))
        col1 = offsetof(LSTFORMAT, words[1]);
    else
        col1 = offsetof(LSTFORMAT, source);

    if (strlen(binline) >= col1) {
        list_flush();
        listline[0] = '\0';
        sprintf(binline, "%*s%6.6o", (int)offsetof(LSTFORMAT, pc), "", addr);
        padto(binline, offsetof(LSTFORMAT, words));
    } else if (strlen(binline) <= col2) {
        sprintf(binline, "%*s%*d %6.6o", (int)SIZEOF_MEMBER(LSTFORMAT, flag), "",
                (int)SIZEOF_MEMBER(LSTFORMAT, line_number), str->line, addr);
        padto(binline, offsetof(LSTFORMAT, words));
    }
}

/* list_value is used to show a computed value */

void list_value(
    STREAM *str,
    unsigned word)
{
    if (dolist()) {
        /* Print the value and go */
        binline[0] = 0;
        sprintf(binline, "%*s%*d         %6.6o", (int)SIZEOF_MEMBER(LSTFORMAT, flag), "",
                (int)SIZEOF_MEMBER(LSTFORMAT, line_number), str->line, word & 0177777);
    }
}

/* Print a word to the listing file */

void list_word(
    STREAM *str,
    unsigned addr,
    unsigned value,
    int size,
    char *flags)
{
    if (dolist()) {
        list_fit(str, addr);
        if (size == 1)
            sprintf(binline + strlen(binline), "   %3.3o%1.1s ", value & 0377, flags);
        else
            sprintf(binline + strlen(binline), "%6.6o%1.1s ", value & 0177777, flags);
    }
}


/* Print just a line with the address to the listing file */

void list_location(
    STREAM *str,
    unsigned addr)
{
    if (dolist()) {
        list_fit(str, addr);
    }
}



/* reports errors */
void report(
    STREAM *str,
    char *fmt,
    ...)
{
    va_list         ap;
    char           *name = "**";
    int             line = 0;

    report_errcnt++;
    if (enabl_debug)
        UPD_DEBUG_SYM(DEBUG_SYM_ERRCNT, report_errcnt);

    errline++;
    if (!pass && list_pass_0 < 2)
        return;                        /* Don't report now. */
    
    if (str) {
        name = str->name;
        line = str->line;
    }

    if (list_line_act & LIST_PAGE_BEFORE)
        list_throw_page();

    fprintf(stderr, "%s:%d: ***ERROR ", name, line);
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);

    if (lstfile && lstfile != stdout) {
        fprintf(lstfile, "%s:%d: ***ERROR ", name, line);
        va_start(ap, fmt);
        vfprintf(lstfile, fmt, ap);
        va_end(ap);
    }
}
