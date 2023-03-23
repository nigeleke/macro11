#ifndef LISTING__H
#define LISTING__H

#include "stream2.h"

/*
    format of a listing line
    Interestingly, no instances of this struct are ever created.
    It lives to be a way to layout the format of a list line.
    I wonder if I should have bothered.
*/

typedef struct lstformat {
    char            flag[2];         /* Error flags */
    char            line_number[6];  /* Line number */
    char            pc[8];           /* Location */
    char            words[8][3];     /* three instruction words */
    char            source[1];       /* source line */
} LSTFORMAT;


/* GLOBAL VARIABLES */
#ifndef  LISTING__C
extern int      list_md;        /* option to list macro/rept definition = yes */

extern int      list_me;        /* option to list macro/rept expansion = yes */

extern int      list_bex;       /* option to show binary */

extern int      list_level;     /* Listing control level.  .LIST
                                   increments; .NLIST decrements */

extern char    *list_page_fmt;  /* Format to use for the page throw */

extern int      list_page_top;  /* Are we at the top of a page? */

extern int      list_line_act;  /* Action to perform when listing the current line */

extern FILE    *lstfile;        /* Listing file descriptor */

extern int      list_pass_0;    /* Also list what happens during the first pass */

extern int      report_errcnt;  /* Count the number of times report() has been called */

#endif


#define LIST_SUPPRESS_LINE  1   /* Suppress the line itself (e.g. '.PAGE') */
#define LIST_PAGE_BEFORE    2   /* New page BEFORE listing the line */
#define LIST_PAGE_AFTER     4   /* New page AFTER listing the line */


void            list_word(
    STREAM *str,
    unsigned addr,
    unsigned value,
    int size,
    char *flags);

void            list_value(
    STREAM *str,
    unsigned word);

void            list_location(
    STREAM *str,
    unsigned word);

void            list_source(
    STREAM *str,
    char *cp);

void            list_throw_page(
    void);

void            list_flush(
    void);

/* TODO: Implement report_err() & report_warn() & report_fatal() */

//#define         report_err   report
//#define         report_warn  report
//#define         report_fatal report

#define         REPORT_WARNING  1  /* Will [optionally] be displayed on pass 2 (and pass 1 if -yl1*2) */
#define         REPORT_ERROR    2  /* Will be displayed on pass 2 (and pass 1 if -yl1*2) */
#define         REPORT_FATAL    3  /* Will be displayed on pass 1 and pass 2 */

#define         report_err(str, fmt, ...)   report(str, fmt, __VA_ARGS__)
#define         report_warn(str, fmt, ...)  report(str, fmt, __VA_ARGS__)
#define         report_fatal(str, fmt, ...) report(str, fmt, __VA_ARGS__)

void            report(
    STREAM *str,
    char *fmt,
    ...);

#endif /* LISTING__H */
