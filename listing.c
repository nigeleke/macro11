#define LISTING__C

#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <stdarg.h>

#include "listing.h"                /* My own definitions */

#include "util.h"
#include "assemble_globals.h"


/* GLOBAL VARIABLES */

int             list_md = 1;        /* option to list macro/rept definition = yes */

int             list_me = 1;        /* option to list macro/rept expansion = yes */

int             list_bex = 1;       /* option to show binary */

int             list_level = 1;     /* Listing control level.  .LIST
                                     * increments; .NLIST decrements */

static char    *list_page_fmt = "\n\n";  /* Format to use for the page throw */

int             list_page_top;      /* Are we at the top of a page? */

int             list_line_act;      /* Action to perform when listing the current line */

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

        if (!binline)
            binline = memcheck(malloc(sizeof(LSTFORMAT) + 16));

        sprintf(binline, "%*s%*d", (int)SIZEOF_MEMBER(LSTFORMAT, flag), "", (int)SIZEOF_MEMBER(LSTFORMAT, line_number),
                str->line);
    }
}

/* list_throw_page starts a new page on the listing */
/* Note: Unlike MACRO-11 blank lines at the top of the page will be suppressed.
 *       For example, a EMPTY lines following a .INCLUDE will not be listed.
 *       Or, indeed, all of the first EMPTY lines in a file.  */

void list_throw_page(
    void)
{
    if (dolist() && !list_page_top) {
        fputs(list_page_fmt, lstfile);
    }
    list_page_top = 1;
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

        if (list_line_act & LIST_SUPPRESS_LINE) {
            list_line_act &= ~LIST_SUPPRESS_LINE;  /* Defer a LIST_PAGE_BEFORE */
        } else {
            if (list_line_act & LIST_PAGE_BEFORE) {
                list_line_act &= ~LIST_PAGE_BEFORE;
                list_throw_page();
            }
            padto(binline, offsetof(LSTFORMAT, source));
            fputs(binline, lstfile);
            fputs(listline, lstfile);
            fputc('\n', lstfile);
            list_page_top = 0;
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
    size_t          col1 = offsetof(LSTFORMAT, source);
    size_t          col2 = offsetof(LSTFORMAT, pc);

    if (strlen(binline) >= col1) {
        list_flush();
        listline[0] = 0;
        binline[0] = 0;
        sprintf(binline, "%*s %6.6o", (int)offsetof(LSTFORMAT, pc), "", addr);
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
        sprintf(binline, "%*s%*d        %6.6o", (int)SIZEOF_MEMBER(LSTFORMAT, flag), "",
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
