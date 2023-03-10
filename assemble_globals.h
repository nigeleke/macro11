
#ifndef ASSEMBLE_GLOBALS__H
#define ASSEMBLE_GLOBALS__H


#include "mlb.h"
#include "symbols.h"
#include "extree.h"



#define START_LOCSYM 30000             /* Start locally generated symbols at 30000$ */
#define MAX_LOCSYM 65535               /* Strictly, local symbols are 1$ to 65535$ */
#define BAD_LOCSYM 999999              /* No local symbol may ever be higher than this */
#define MAX_MLBS 32                    /* Number of macro libraries */

#define MAX_CONDS 256
typedef struct cond {
    int             ok;         /* What the condition evaluated to */
    char           *file;       /* What file and line it occurred */
    int             line;
} COND;

#define SECT_STACK_SIZE 32

#ifndef ASSEMBLE_GLOBALS__C
/* GLOBAL VARIABLES */
extern int      pass;           /* The current assembly pass.  0 = first pass */
extern int      stmtno;         /* The current source line number */
extern int      radix;          /* The current input conversion radix */
extern int      lsb;            /* The current local symbol section identifier */
extern int      lsb_used;       /* Whether there was a local symbol using this lsb */
extern int      next_lsb;       /* The number of the next local symbol block */
extern int      last_macro_lsb; /* The last block in which a macro
                                   automatic label was created */

extern int      last_locsym;    /* The last local symbol number generated */

extern int      enabl_debug;    /* Whether assembler debugging is enabled */

extern int      strictness;              /* How strict (relaxed) do we want to be? */
                                         /* <0 = relaxed, 0 = normal, >0 = strict */

#ifndef STRINGENTNESS 
#define STRINGENTNESS 4                  /* Strictness level we consider to be STRINGENT (0-4) */
#endif
#define STRINGENT     (strictness >= STRINGENTNESS)  /* As STRICTEST but also follow the MACRO-11 documentation */

#if (STRINGENTNESS > 3)
#define STRICT        (strictness >  0)  /* Close to MACRO-11 V05.05 syntax */
#define STRICTER      (strictness >  1)  /* As close as we like or even more */
#define STRICTEST     (strictness >  2)  /* Really mega-strict (e.g. .END required) */
#elif (STRINGENTNESS > 2)
#define STRICT        (strictness >= 0)  /* Close to MACRO-11 V05.05 syntax */
#define STRICTER      (strictness >  0)  /* As close as we like or even more */
#define STRICTEST     (strictness >  1)  /* Really mega-strict (e.g. .END required) */
#elif (STRINGENTNESS > 1)
#define STRICT        (strictness >= 0)  /* Close to MACRO-11 V05.05 syntax */
#define STRICTER      (strictness >= 0)  /* As close as we like or even more */
#define STRICTEST     (strictness >  0)  /* Really mega-strict (e.g. .END required) */
#else
#define STRICT        (strictness >= 0)  /* Close to MACRO-11 V05.05 syntax */
#define STRICTER      (strictness >= 0)  /* As close as we like or even more */
#define STRICTEST     (strictness >= 0)  /* Really mega-strict (e.g. .END required) */
#endif

#define RELAXED       (strictness <  0)  /* Relax the rules as much as we like */
#define VERY_RELAXED  (strictness < -1)  /* Relax the rules so much that even .END isn't the end */

extern int      opt_enabl_ama;  /* May be changed by command line */

extern int      enabl_ama;      /* When set, chooses absolute (037) versus
                                   PC-relative */
                                /* (067) addressing mode */
extern int      enabl_lsb;      /* When set, stops non-local symbol
                                   definitions from delimiting local
                                   symbol sections. */

extern int      enabl_gbl;      /* Implicit definition of global symbols */

extern int      enabl_lc;       /* If lowercase disabled, convert assembler
                                   source to upper case. */
extern int      enabl_lcm;      /* If lowercase disabled, .IF IDN/DIF are
                                   case-sensitive. */
extern int      suppressed;     /* Assembly suppressed by failed conditional */

extern MLB     *mlbs[MAX_MLBS]; /* macro libraries specified on the command line */
extern int      nr_mlbs;        /* Number of macro libraries */

extern int      enabl_mcl;      /* If MCALL of unknown symbols is enabled. */

extern COND     conds[MAX_CONDS];       /* Stack of recent conditions */
extern int      last_cond;      /* 0 means no stacked cond. */

extern SECTION *sect_stack[SECT_STACK_SIZE]; /* 32 saved sections */
extern int      dot_stack[SECT_STACK_SIZE];  /* 32 saved sections */
extern int      sect_sp;        /* Stack pointer */

extern char    *module_name;    /* The module name (taken from the 'TITLE'); */

extern unsigned *ident;         /* .IDENT name (encoded RAD50 value) */

extern EX_TREE *xfer_address;   /* The transfer address */

extern SYMBOL  *current_pc;     /* The current program counter */

extern unsigned last_dot_addr;  /* Last coded PC... */
extern SECTION *last_dot_section;       /* ...and its program section */

/* The following are dummy psects for symbols which have meaning to
   the assembler: */
extern SECTION  register_section;
extern SECTION  pseudo_section; /* the section containing the  pseudo-operations */
extern SECTION  instruction_section;    /* the section containing instructions */
extern SECTION  macro_section;  /* Section for macros */

/* These are real psects that get written out to the object file */
extern SECTION  absolute_section;       /* The default  absolute section */
extern SECTION  blank_section;
extern SECTION *sections[256];  /* Array of sections in the order they were defined */
extern int      sector;         /* number of such sections */

#endif


#endif
