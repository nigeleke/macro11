
#define SYMBOLS__C

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "symbols.h"                   /* my own definitions */

#include "util.h"
#include "assemble_globals.h"
#include "listing.h"
#include "object.h"

/* GLOBALS */
int             symbol_len = SYMMAX_DEFAULT;    /* max. len of symbols. default = 6 */
int             symbol_allow_underscores = 0;   /* allow "_" in symbol names */

SYMBOL         *reg_sym[9];     /* Keep the register symbols in a handy array */


SYMBOL_TABLE    system_st;      /* System symbols (Instructions,
                                   pseudo-ops, registers) */

SYMBOL_TABLE    section_st;     /* Program sections */

SYMBOL_TABLE    symbol_st;      /* User symbols */

SYMBOL_TABLE    macro_st;       /* Macros */

SYMBOL_TABLE    implicit_st;    /* The symbols which may be implicit globals */

SYMBOL_TABLE    undefined_st;   /* The symbols which may be undefined */


void list_section(SECTION *sec);

void dump_sym(
    SYMBOL *sym)
{
    /* TODO: Replace report_warn() with something like report_info() */
    report_warn(NULL, "'%s': %06o, stmt %d, flags %o:%s%s%s%s%s%s%s\n",
                sym->label,
                sym->value,
                sym->stmtno,
                sym->flags,
                ((sym->flags & SYMBOLFLAG_PERMANENT)? " PERMANENT" : ""),
                ((sym->flags & SYMBOLFLAG_GLOBAL)? " GLOBAL" : ""),
                ((sym->flags & SYMBOLFLAG_WEAK)? " WEAK" : ""),
                ((sym->flags & SYMBOLFLAG_DEFINITION)? " DEFINITION" : ""),
                ((sym->flags & SYMBOLFLAG_UNDEFINED)? " UNDEFINED" : ""),
                ((sym->flags & SYMBOLFLAG_LOCAL)? " LOCAL" : ""),
                ((sym->flags & SYMBOLFLAG_IMPLICIT_GLOBAL)? " IMPLICIT_GLOBAL" : ""));
}

void check_sym_invariants(
    SYMBOL *sym,
    char *file,
    int line,
    STREAM *stream)
{
    int dump = 0;

    /* TODO: Replace report_err() throughout with report_fatal() ? */

    if (sym->section == &instruction_section) {
        /* The instructions use the flags field differently */
        if ((sym->flags & ~OC_MASK) != 0) {
            report_err(stream, "%s %d: Instruction symbol %s has wrong flags\n", file, line, sym->label);
            dump_sym(sym);
        }
        return;
    }

    /*
     * A symbol is GLOBAL if it appears in the object file's symbol table.
     * It can be either exported (if defined) or imported (if not defined).
     *
     * A common test like this
     *
     *  if ((sym->flags & (SYMBOLFLAG_GLOBAL | SYMBOLFLAG_DEFINITION)) == SYMBOLFLAG_GLOBAL)
     *
     * tests if a symbol is imported.
     */

    switch (sym->flags & (SYMBOLFLAG_PERMANENT|SYMBOLFLAG_GLOBAL|SYMBOLFLAG_DEFINITION|SYMBOLFLAG_UNDEFINED)) {
        /* A DEFINITION can independently be PERMANENT and/or GLOBAL */
        case SYMBOLFLAG_PERMANENT|SYMBOLFLAG_GLOBAL|SYMBOLFLAG_DEFINITION:
        case                      SYMBOLFLAG_GLOBAL|SYMBOLFLAG_DEFINITION:
        case SYMBOLFLAG_PERMANENT|                  SYMBOLFLAG_DEFINITION:
        case                                        SYMBOLFLAG_DEFINITION:
            break;
        /* A GLOBAL can also be undefined, but then it's still usable */
        case                      SYMBOLFLAG_GLOBAL:
            break;
        /* A truly UNDEFINED symbol is an error to use */
        /* (this seems logically equivalent to all of these flags cleared) */
        case SYMBOLFLAG_UNDEFINED:
            break;
        default:
            report_err(stream, "%s %d: Symbol '%s' in section '%s' definedness is inconsistent\n",
                       file, line, sym->label, sym->section->label);
            dump++;
    }

    if ( (sym->flags & SYMBOLFLAG_IMPLICIT_GLOBAL) &&
        !(sym->flags & SYMBOLFLAG_GLOBAL)) {
        report_err(stream, "%s %d: Symbol '%s' globalness is inconsistent\n", file, line, sym->label);
        dump++;
    }

    if ( (sym->flags & SYMBOLFLAG_LOCAL) &&
         (sym->flags & SYMBOLFLAG_GLOBAL)) {
        report_err(stream, "%s %d: Symbol '%s' is local and global\n", file, line, sym->label);
        dump++;
    }

    if ( (sym->flags & SYMBOLFLAG_PERMANENT) &&
        !(sym->flags & SYMBOLFLAG_DEFINITION)) {
        report_err(stream, "%s %d: Symbol '%s' is permanent without definition\n", file, line, sym->label);
        dump++;
    }

    if ( (sym->flags & SYMBOLFLAG_WEAK) &&
        !(sym->flags & SYMBOLFLAG_GLOBAL)) {
        report_err(stream, "%s %d: Symbol '%s' weak/global is inconsistent\n", file, line, sym->label);
        dump++;
    }

    if (sym->value > 0xffff) {
            report_err(stream, "%s %d: Symbol '%s' [%o] takes more than 16-bits\n",
                       file, line, sym->label, sym->value);
            dump++;
    }

    if (enabl_debug && dump) {
        dump_sym(sym);
    }
}

/* hash_name hashes a name into a value from 0-HASH_SIZE */

int hash_name(
    char *label)
{
    unsigned        accum = 0;

    while (*label)
        accum = (accum << 1) ^ *label++;

    accum %= HASH_SIZE;

    return accum;
}



/* Diagnostic: symflags returns a char* which gives flags I can use to
   show the context of a symbol. */

char           *symflags(
    SYMBOL *sym)
{
    static char     temp[8];
    char           *fp = temp;

    /* TODO: Update with the rest of the SYMBOLFLAGs */

    if (sym->flags & SYMBOLFLAG_GLOBAL)
        *fp++ = 'G';
    if (sym->flags & SYMBOLFLAG_PERMANENT)
        *fp++ = 'P';
    if (sym->flags & SYMBOLFLAG_DEFINITION)
        *fp++ = 'D';
    *fp = 0;
    return fp;
}



/* Allocate a new symbol.  Does not add it to any symbol table. */

static SYMBOL  *new_sym(
    char *label)
{
    SYMBOL         *sym = memcheck(malloc(sizeof(SYMBOL)));

    sym->label = memcheck(strdup(label));
    sym->section = NULL;
    sym->value = 0;
    sym->flags = 0;
    return sym;
}

/* Free a symbol. Does not remove it from any symbol table.  */

void free_sym(
    SYMBOL *sym)
{
    if (sym->label) {
        check_sym_invariants(sym, __FILE__, __LINE__, NULL);
        free(sym->label);
        sym->label = NULL;
    }
    free(sym);
}

/* remove_sym removes a symbol from its symbol table. */

void remove_sym(
    SYMBOL *sym,
    SYMBOL_TABLE *table)
{
    SYMBOL        **prevp,
                   *symp;
    int             hash;

    check_sym_invariants(sym, __FILE__, __LINE__, NULL);
    hash = hash_name(sym->label);
    prevp = &table->hash[hash];
    while (symp = *prevp, symp != NULL && symp != sym)
        prevp = &symp->next;

    if (symp)
        *prevp = sym->next;
}

/* lookup_sym finds a symbol in a table */

SYMBOL         *lookup_sym(
    char *label,
    SYMBOL_TABLE *table)
{
    unsigned        hash;
    SYMBOL         *sym;

    hash = hash_name(label);

    sym = table->hash[hash];
    while (sym && strcmp(sym->label, label) != 0)
        sym = sym->next;

    if (sym) {
        check_sym_invariants(sym, __FILE__, __LINE__, NULL);
    }
    return sym;
}

/* next_sym - returns the next symbol from a symbol table.  Must be
   preceeded by first_sym.  Returns NULL after the last symbol. */

SYMBOL         *next_sym(
    SYMBOL_TABLE *table,
    SYMBOL_ITER *iter)
{
    if (iter->current)
        iter->current = iter->current->next;

    while (iter->current == NULL) {
        if (iter->subscript >= HASH_SIZE)
            return NULL;               /* No more symbols. */
        iter->current = table->hash[iter->subscript];
        iter->subscript++;
    }

    return iter->current;              /* Got a symbol. */
}

/* first_sym - returns the first symbol from a symbol table. Symbols
   are stored in random order. */

SYMBOL         *first_sym(
    SYMBOL_TABLE *table,
    SYMBOL_ITER *iter)
{
    iter->subscript = 0;
    iter->current = NULL;
    return next_sym(table, iter);
}

/* add_table - add a symbol to a symbol table. */

void add_table(
    SYMBOL *sym,
    SYMBOL_TABLE *table)
{
    int             hash = hash_name(sym->label);

    sym->next = table->hash[hash];
    table->hash[hash] = sym;
    check_sym_invariants(sym, __FILE__, __LINE__, NULL);
}

/* add_sym - used throughout to add or update symbols in a symbol
   table.  */

SYMBOL         *add_sym(
    char *labelraw,
    unsigned value,
    unsigned flags,
    SECTION *section,
    SYMBOL_TABLE *table,
    STREAM *stream)
{
    SYMBOL         *sym;
    char            label[SYMMAX_MAX + 1];
    int             is_local = isdigit((unsigned char)labelraw[0]);

    if (is_local) {
        // Don't truncate local labels
        strncpy(label, labelraw, SYMMAX_MAX);
        label[SYMMAX_MAX] = 0;
    } else {
        //JH: truncate symbol to SYMMAX
        strncpy(label, labelraw, symbol_len);
        label[symbol_len] = 0;
    }

    /* For directives with more than 6 characters ...
     * ... and -ysl >6 and not -stringent ...
     * ... add_sym() all of the shorter names ...
     * ... e.g. .INCLU & .INCLUD as well as .INCLUDE
     */

    if (section == &pseudo_section) {
        if (!STRINGENT && symbol_len > 6) {
            if (strlen(label) > 6) {
               label[strlen(label)-1] = '\0';
               if (add_sym(label, value, flags, section, table, stream) == NULL)
                   return NULL;  /* Should never happen */
                strncpy(label, labelraw, symbol_len);
                label[symbol_len] = '\0';
            }
        }
    }

    sym = lookup_sym(label, table);
    if (sym == NULL) {
        if (is_local && !(flags & SYMBOLFLAG_LOCAL)) {
            /* This would be an internal error and should never happen */
            report_warn(stream, "Symbol '%s' is local but not defined LOCAL\n", label);
            dump_sym(sym);
            flags |=  SYMBOLFLAG_LOCAL;   /* Force this to become a local symbol */
        }
    } else {
        // A symbol registered as "undefined" can be changed.
        //
        check_sym_invariants(sym, __FILE__, __LINE__, stream);

        if (sym->flags & SYMBOLFLAG_PERMANENT) {
            if (flags & SYMBOLFLAG_PERMANENT) {
                if (sym->value != value || sym->section != section) {
                    report_err(stream, "Phase error: '%s'\n", label);
                    return NULL;
                }
            } else {
                report_warn(stream, "Redefining permanent symbol '%s'\n", label);
                return NULL;
            }
        }

        if ((sym->flags & SYMBOLFLAG_UNDEFINED) && !(flags & SYMBOLFLAG_UNDEFINED)) {
            sym->flags &= ~(SYMBOLFLAG_PERMANENT | SYMBOLFLAG_UNDEFINED);
        }
        else if (!(sym->flags & SYMBOLFLAG_UNDEFINED) && (flags & SYMBOLFLAG_UNDEFINED)) {
            report_err(stream, "INTERNAL ERROR: Turning defined symbol '%s' into undefined\n", label);
            return sym;
        }
        /* Check for compatible definition */
        else if (sym->section == section && sym->value == value) {
            sym->flags |= flags;       /* Merge flags quietly */
            check_sym_invariants(sym, __FILE__, __LINE__, stream);
            return sym;                /* 's okay */
        }

        if (!(sym->flags & SYMBOLFLAG_PERMANENT)) {
            /* permit redefinition */
            sym->value = value;
            sym->flags |= flags;
            sym->section = section;
            check_sym_invariants(sym, __FILE__, __LINE__, stream);
            return sym;
        }

        report_err(stream, "INTERNAL ERROR: Bad symbol '%s' redefinition\n", label);
        return NULL;                   /* Bad symbol redefinition */
    }

    sym = new_sym(label);
    sym->flags = flags;
    sym->stmtno = stmtno;
    sym->section = section;
    sym->value = value;

    add_table(sym, table);

    return sym;
}

/* add_symbols adds all the internal symbols. */

void add_symbols(
    SECTION *current_section)
{
    current_pc = add_sym(".", 0, SYMBOLFLAG_DEFINITION, current_section, &symbol_st, NULL);

/**********************************************************************/

#define ADD_SYM_REGISTER(label, value) \
            add_sym(label, value, (SYMBOLFLAG_PERMANENT | SYMBOLFLAG_DEFINITION), \
                    &register_section, &system_st, NULL)

    reg_sym[0] = ADD_SYM_REGISTER("R0", 0);
    reg_sym[1] = ADD_SYM_REGISTER("R1", 1);
    reg_sym[2] = ADD_SYM_REGISTER("R2", 2);
    reg_sym[3] = ADD_SYM_REGISTER("R3", 3);
    reg_sym[4] = ADD_SYM_REGISTER("R4", 4);
    reg_sym[5] = ADD_SYM_REGISTER("R5", 5);
    reg_sym[6] = ADD_SYM_REGISTER("SP", 6);
    reg_sym[7] = ADD_SYM_REGISTER("PC", 7);
    REG_ERR    = ADD_SYM_REGISTER("%E", REG_ERR_VALUE);  /* Used for invalid register-number (%0-%7) usage */

#undef ADD_SYM_REGISTER

/**********************************************************************/

#define ADD_SYM_PSEUDO(label, value) \
            add_sym(label, value, (SYMBOLFLAG_PERMANENT | SYMBOLFLAG_DEFINITION), \
                    &pseudo_section, &system_st, NULL)

    /* Symbols longer than current SYMMAX will be truncated. SYMMAX=6 is minimum! */

    ADD_SYM_PSEUDO(".ASCII",   P_ASCII);
    ADD_SYM_PSEUDO(".ASCIZ",   P_ASCIZ);
    ADD_SYM_PSEUDO(".ASECT",   P_ASECT);
    ADD_SYM_PSEUDO(".BLKB",    P_BLKB);
    ADD_SYM_PSEUDO(".BLKW",    P_BLKW);
    ADD_SYM_PSEUDO(".BYTE",    P_BYTE);
    ADD_SYM_PSEUDO(".CROSS",   P_CROSS);
    ADD_SYM_PSEUDO(".CSECT",   P_CSECT);
    ADD_SYM_PSEUDO(".DSABL",   P_DSABL);
    ADD_SYM_PSEUDO(".ENABL",   P_ENABL);
    ADD_SYM_PSEUDO(".END",     P_END);
    ADD_SYM_PSEUDO(".ENDC",    P_ENDC);
    ADD_SYM_PSEUDO(".ENDM",    P_ENDM);
    ADD_SYM_PSEUDO(".ENDR",    P_ENDR);
    ADD_SYM_PSEUDO(".EOT",     P_EOT);
    ADD_SYM_PSEUDO(".ERROR",   P_ERROR);
    ADD_SYM_PSEUDO(".EVEN",    P_EVEN);
    ADD_SYM_PSEUDO(".FLT2",    P_FLT2);
    ADD_SYM_PSEUDO(".FLT4",    P_FLT4);
    ADD_SYM_PSEUDO(".GLOBL",   P_GLOBL);
    ADD_SYM_PSEUDO(".IDENT",   P_IDENT);
    ADD_SYM_PSEUDO(".IF",      P_IF);
    ADD_SYM_PSEUDO(".IFF",     P_IFF);
    ADD_SYM_PSEUDO(".IFT",     P_IFT);
    ADD_SYM_PSEUDO(".IFTF",    P_IFTF);
    ADD_SYM_PSEUDO(".IFDF",    P_IFXX);
    ADD_SYM_PSEUDO(".IFNDF",   P_IFXX);
    ADD_SYM_PSEUDO(".IFZ",     P_IFXX);
    ADD_SYM_PSEUDO(".IFEQ",    P_IFXX);
    ADD_SYM_PSEUDO(".IFNZ",    P_IFXX);
    ADD_SYM_PSEUDO(".IFNE",    P_IFXX);
    ADD_SYM_PSEUDO(".IFL",     P_IFXX);
    ADD_SYM_PSEUDO(".IFLT",    P_IFXX);
    ADD_SYM_PSEUDO(".IFG",     P_IFXX);
    ADD_SYM_PSEUDO(".IFGT",    P_IFXX);
    ADD_SYM_PSEUDO(".IFLE",    P_IFXX);
    ADD_SYM_PSEUDO(".IFGE",    P_IFXX);
    ADD_SYM_PSEUDO(".IIF",     P_IIF);
    ADD_SYM_PSEUDO(".INCLUDE", P_INCLUDE);
    ADD_SYM_PSEUDO(".IRP",     P_IRP);
    ADD_SYM_PSEUDO(".IRPC",    P_IRPC);
    ADD_SYM_PSEUDO(".LIBRARY", P_LIBRARY);
    ADD_SYM_PSEUDO(".LIMIT",   P_LIMIT);
    ADD_SYM_PSEUDO(".LIST",    P_LIST);
    ADD_SYM_PSEUDO(".MACRO",   P_MACRO);
    ADD_SYM_PSEUDO(".MCALL",   P_MCALL);
    ADD_SYM_PSEUDO(".MDELETE", P_MDELETE);
    ADD_SYM_PSEUDO(".MEXIT",   P_MEXIT);
    ADD_SYM_PSEUDO(".NARG",    P_NARG);
    ADD_SYM_PSEUDO(".NCHR",    P_NCHR);
    ADD_SYM_PSEUDO(".NLIST",   P_NLIST);
    ADD_SYM_PSEUDO(".NOCROSS", P_NOCROSS);
    ADD_SYM_PSEUDO(".NTYPE",   P_NTYPE);
    ADD_SYM_PSEUDO(".ODD",     P_ODD);
    ADD_SYM_PSEUDO(".PACKED",  P_PACKED);
    ADD_SYM_PSEUDO(".PAGE",    P_PAGE);
    ADD_SYM_PSEUDO(".PRINT",   P_PRINT);
    ADD_SYM_PSEUDO(".PSECT",   P_PSECT);
    ADD_SYM_PSEUDO(".RAD50",   P_RAD50);
    ADD_SYM_PSEUDO(".RADIX",   P_RADIX);
    ADD_SYM_PSEUDO(".REM",     P_REM);
    ADD_SYM_PSEUDO(".REPT",    P_REPT);
    ADD_SYM_PSEUDO(".RESTORE", P_RESTORE);
    ADD_SYM_PSEUDO(".SAVE",    P_SAVE);
    ADD_SYM_PSEUDO(".SBTTL",   P_SBTTL);
    ADD_SYM_PSEUDO(".TITLE",   P_TITLE);
    ADD_SYM_PSEUDO(".WEAK",    P_WEAK);
    ADD_SYM_PSEUDO(".WORD",    P_WORD);

#undef ADD_SYM_PSEUDO

/**********************************************************************/

#define ADD_SYM_INST(label, value, operand) \
            add_sym(label, value, operand, \
                    &instruction_section, &system_st, NULL)

    ADD_SYM_INST("ADC",    I_ADC,    OC_1GEN);
    ADD_SYM_INST("ADCB",   I_ADCB,   OC_1GEN);
    ADD_SYM_INST("ADD",    I_ADD,    OC_2GEN);
    ADD_SYM_INST("ASH",    I_ASH,    OC_ASH);
    ADD_SYM_INST("ASHC",   I_ASHC,   OC_ASH);
    ADD_SYM_INST("ASL",    I_ASL,    OC_1GEN);
    ADD_SYM_INST("ASLB",   I_ASLB,   OC_1GEN);
    ADD_SYM_INST("ASR",    I_ASR,    OC_1GEN);
    ADD_SYM_INST("ASRB",   I_ASRB,   OC_1GEN);
    ADD_SYM_INST("BCC",    I_BCC,    OC_BR);
    ADD_SYM_INST("BCS",    I_BCS,    OC_BR);
    ADD_SYM_INST("BEQ",    I_BEQ,    OC_BR);
    ADD_SYM_INST("BGE",    I_BGE,    OC_BR);
    ADD_SYM_INST("BGT",    I_BGT,    OC_BR);
    ADD_SYM_INST("BHI",    I_BHI,    OC_BR);
    ADD_SYM_INST("BHIS",   I_BHIS,   OC_BR);
    ADD_SYM_INST("BIC",    I_BIC,    OC_2GEN);
    ADD_SYM_INST("BICB",   I_BICB,   OC_2GEN);
    ADD_SYM_INST("BIS",    I_BIS,    OC_2GEN);
    ADD_SYM_INST("BISB",   I_BISB,   OC_2GEN);
    ADD_SYM_INST("BIT",    I_BIT,    OC_2GEN);
    ADD_SYM_INST("BITB",   I_BITB,   OC_2GEN);
    ADD_SYM_INST("BLE",    I_BLE,    OC_BR);
    ADD_SYM_INST("BLO",    I_BLO,    OC_BR);
    ADD_SYM_INST("BLOS",   I_BLOS,   OC_BR);
    ADD_SYM_INST("BLT",    I_BLT,    OC_BR);
    ADD_SYM_INST("BMI",    I_BMI,    OC_BR);
    ADD_SYM_INST("BNE",    I_BNE,    OC_BR);
    ADD_SYM_INST("BPL",    I_BPL,    OC_BR);
    ADD_SYM_INST("BPT",    I_BPT,    OC_NONE);
    ADD_SYM_INST("BR",     I_BR,     OC_BR);
    ADD_SYM_INST("BVC",    I_BVC,    OC_BR);
    ADD_SYM_INST("BVS",    I_BVS,    OC_BR);
    ADD_SYM_INST("CALL",   I_CALL,   OC_1GEN);
    ADD_SYM_INST("CALLR",  I_CALLR,  OC_1GEN);
    ADD_SYM_INST("CCC",    I_CCC,    OC_NONE);
    ADD_SYM_INST("CLC",    I_CLC,    OC_NONE);
    ADD_SYM_INST("CLN",    I_CLN,    OC_NONE);
    ADD_SYM_INST("CLR",    I_CLR,    OC_1GEN);
    ADD_SYM_INST("CLRB",   I_CLRB,   OC_1GEN);
    ADD_SYM_INST("CLV",    I_CLV,    OC_NONE);
    ADD_SYM_INST("CLZ",    I_CLZ,    OC_NONE);
    ADD_SYM_INST("CMP",    I_CMP,    OC_2GEN);
    ADD_SYM_INST("CMPB",   I_CMPB,   OC_2GEN);
    ADD_SYM_INST("COM",    I_COM,    OC_1GEN);
    ADD_SYM_INST("COMB",   I_COMB,   OC_1GEN);
    ADD_SYM_INST("CSM",    I_CSM,    OC_1GEN);
    ADD_SYM_INST("DEC",    I_DEC,    OC_1GEN);
    ADD_SYM_INST("DECB",   I_DECB,   OC_1GEN);
    ADD_SYM_INST("DIV",    I_DIV,    OC_ASH);
    ADD_SYM_INST("EMT",    I_EMT,    OC_MARK);
    ADD_SYM_INST("FADD",   I_FADD,   OC_1REG);
    ADD_SYM_INST("FDIV",   I_FDIV,   OC_1REG);
    ADD_SYM_INST("FMUL",   I_FMUL,   OC_1REG);
    ADD_SYM_INST("FSUB",   I_FSUB,   OC_1REG);
    ADD_SYM_INST("HALT",   I_HALT,   OC_NONE);
    ADD_SYM_INST("INC",    I_INC,    OC_1GEN);
    ADD_SYM_INST("INCB",   I_INCB,   OC_1GEN);
    ADD_SYM_INST("IOT",    I_IOT,    OC_NONE);
    ADD_SYM_INST("JMP",    I_JMP,    OC_1GEN);
    ADD_SYM_INST("JSR",    I_JSR,    OC_JSR);
    ADD_SYM_INST("MARK",   I_MARK,   OC_MARK);
    ADD_SYM_INST("MED6X",  I_MED6X,  OC_NONE);  /* PDP-11/60 Maintenance */
    ADD_SYM_INST("MED74C", I_MED74C, OC_NONE);  /* PDP-11/74 CIS Maintenance */
    ADD_SYM_INST("MFPD",   I_MFPD,   OC_1GEN);
    ADD_SYM_INST("MFPI",   I_MFPI,   OC_1GEN);
    ADD_SYM_INST("MFPS",   I_MFPS,   OC_1GEN);
    ADD_SYM_INST("MFPT",   I_MFPT,   OC_NONE);
    ADD_SYM_INST("MOV",    I_MOV,    OC_2GEN);
    ADD_SYM_INST("MOVB",   I_MOVB,   OC_2GEN);
    ADD_SYM_INST("MTPD",   I_MTPD,   OC_1GEN);
    ADD_SYM_INST("MTPI",   I_MTPI,   OC_1GEN);
    ADD_SYM_INST("MTPS",   I_MTPS,   OC_1GEN);
    ADD_SYM_INST("MUL",    I_MUL,    OC_ASH);
    ADD_SYM_INST("NEG",    I_NEG,    OC_1GEN);
    ADD_SYM_INST("NEGB",   I_NEGB,   OC_1GEN);
    ADD_SYM_INST("NOP",    I_NOP,    OC_NONE);
    ADD_SYM_INST("RESET",  I_RESET,  OC_NONE);
    ADD_SYM_INST("RETURN", I_RETURN, OC_NONE);
    ADD_SYM_INST("ROL",    I_ROL,    OC_1GEN);
    ADD_SYM_INST("ROLB",   I_ROLB,   OC_1GEN);
    ADD_SYM_INST("ROR",    I_ROR,    OC_1GEN);
    ADD_SYM_INST("RORB",   I_RORB,   OC_1GEN);
    ADD_SYM_INST("RTI",    I_RTI,    OC_NONE);
    ADD_SYM_INST("RTS",    I_RTS,    OC_1REG);
    ADD_SYM_INST("RTT",    I_RTT,    OC_NONE);
    ADD_SYM_INST("SBC",    I_SBC,    OC_1GEN);
    ADD_SYM_INST("SBCB",   I_SBCB,   OC_1GEN);
    ADD_SYM_INST("SCC",    I_SCC,    OC_NONE);
    ADD_SYM_INST("SEC",    I_SEC,    OC_NONE);
    ADD_SYM_INST("SEN",    I_SEN,    OC_NONE);
    ADD_SYM_INST("SEV",    I_SEV,    OC_NONE);
    ADD_SYM_INST("SEZ",    I_SEZ,    OC_NONE);
    ADD_SYM_INST("SOB",    I_SOB,    OC_SOB);
    ADD_SYM_INST("SPL",    I_SPL,    OC_MARK);
    ADD_SYM_INST("SUB",    I_SUB,    OC_2GEN);
    ADD_SYM_INST("SWAB",   I_SWAB,   OC_1GEN);
    ADD_SYM_INST("SXT",    I_SXT,    OC_1GEN);
    ADD_SYM_INST("TRAP",   I_TRAP,   OC_MARK);
    ADD_SYM_INST("TST",    I_TST,    OC_1GEN);
    ADD_SYM_INST("TSTB",   I_TSTB,   OC_1GEN);
    ADD_SYM_INST("TSTSET", I_TSTSET, OC_1GEN);
    ADD_SYM_INST("WAIT",   I_WAIT,   OC_NONE);
    ADD_SYM_INST("WRTLCK", I_WRTLCK, OC_1GEN);
    ADD_SYM_INST("XFC",    I_XFC,    OC_NONE);  /* PDP-11/60 Extended Function Code (is not in V05.05) */
    ADD_SYM_INST("XOR",    I_XOR,    OC_JSR);

    /* FPP instructions */

    ADD_SYM_INST("ABSD",   I_ABSD,   OC_FPP_FDST);
    ADD_SYM_INST("ABSF",   I_ABSF,   OC_FPP_FDST);
    ADD_SYM_INST("ADDD",   I_ADDD,   OC_FPP_FSRCAC);
    ADD_SYM_INST("ADDF",   I_ADDF,   OC_FPP_FSRCAC);
    ADD_SYM_INST("CFCC",   I_CFCC,   OC_NONE);
    ADD_SYM_INST("CLRD",   I_CLRD,   OC_FPP_FDST);
    ADD_SYM_INST("CLRF",   I_CLRF,   OC_FPP_FDST);
    ADD_SYM_INST("CMPD",   I_CMPD,   OC_FPP_FSRCAC);
    ADD_SYM_INST("CMPF",   I_CMPF,   OC_FPP_FSRCAC);
    ADD_SYM_INST("DIVD",   I_DIVD,   OC_FPP_FSRCAC);
    ADD_SYM_INST("DIVF",   I_DIVF,   OC_FPP_FSRCAC);
    ADD_SYM_INST("LDCDF",  I_LDCDF,  OC_FPP_FSRCAC);
    ADD_SYM_INST("LDCFD",  I_LDCFD,  OC_FPP_FSRCAC);
    ADD_SYM_INST("LDCID",  I_LDCID,  OC_FPP_SRCAC);
    ADD_SYM_INST("LDCIF",  I_LDCIF,  OC_FPP_SRCAC);
    ADD_SYM_INST("LDCLD",  I_LDCLD,  OC_FPP_SRCAC);
    ADD_SYM_INST("LDCLF",  I_LDCLF,  OC_FPP_SRCAC);
    ADD_SYM_INST("LDD",    I_LDD,    OC_FPP_FSRCAC);
    ADD_SYM_INST("LDEXP",  I_LDEXP,  OC_FPP_SRCAC);
    ADD_SYM_INST("LDF",    I_LDF,    OC_FPP_FSRCAC);
    ADD_SYM_INST("LDFPS",  I_LDFPS,  OC_1GEN);
    ADD_SYM_INST("MODD",   I_MODD,   OC_FPP_FSRCAC);
    ADD_SYM_INST("MODF",   I_MODF,   OC_FPP_FSRCAC);
    ADD_SYM_INST("MULD",   I_MULD,   OC_FPP_FSRCAC);
    ADD_SYM_INST("MULF",   I_MULF,   OC_FPP_FSRCAC);
    ADD_SYM_INST("NEGD",   I_NEGD,   OC_FPP_FDST);
    ADD_SYM_INST("NEGF",   I_NEGF,   OC_FPP_FDST);
    ADD_SYM_INST("SETD",   I_SETD,   OC_NONE);
    ADD_SYM_INST("SETF",   I_SETF,   OC_NONE);
    ADD_SYM_INST("SETI",   I_SETI,   OC_NONE);
    ADD_SYM_INST("SETL",   I_SETL,   OC_NONE);
    ADD_SYM_INST("STA0",   I_STA0,   OC_NONE);        /* 11/45 Maintenance Right Shift */
    ADD_SYM_INST("STB0",   I_STB0,   OC_NONE);        /* Diagnostic Floating Point */
    ADD_SYM_INST("STCDF",  I_STCDF,  OC_FPP_ACFDST);
    ADD_SYM_INST("STCDI",  I_STCDI,  OC_FPP_ACFDST);
    ADD_SYM_INST("STCDL",  I_STCDL,  OC_FPP_ACFDST);
    ADD_SYM_INST("STCFD",  I_STCFD,  OC_FPP_ACFDST);
    ADD_SYM_INST("STCFI",  I_STCFI,  OC_FPP_ACFDST);
    ADD_SYM_INST("STCFL",  I_STCFL,  OC_FPP_ACFDST);
    ADD_SYM_INST("STD",    I_STD,    OC_FPP_ACFDST);
    ADD_SYM_INST("STEXP",  I_STEXP,  OC_FPP_ACDST);
    ADD_SYM_INST("STF",    I_STF,    OC_FPP_ACFDST);
    ADD_SYM_INST("STFPS",  I_STFPS,  OC_1GEN);
    ADD_SYM_INST("STST",   I_STST,   OC_1GEN);
    ADD_SYM_INST("SUBD",   I_SUBD,   OC_FPP_FSRCAC);
    ADD_SYM_INST("SUBF",   I_SUBF,   OC_FPP_FSRCAC);
    ADD_SYM_INST("TSTD",   I_TSTD,   OC_FPP_FDST);
    ADD_SYM_INST("TSTF",   I_TSTF,   OC_FPP_FDST);

    /* The CIS instructions */

    ADD_SYM_INST("ADDN",   I_ADDN,          OC_NONE);
    ADD_SYM_INST("ADDNI",  I_ADDN|I_CIS_I,  OC_CIS3);
    ADD_SYM_INST("ADDP",   I_ADDP,          OC_NONE);
    ADD_SYM_INST("ADDPI",  I_ADDP|I_CIS_I,  OC_CIS3);
    ADD_SYM_INST("ASHN",   I_ASHN,          OC_NONE);
    ADD_SYM_INST("ASHNI",  I_ASHN|I_CIS_I,  OC_CIS3);
    ADD_SYM_INST("ASHP",   I_ASHP,          OC_NONE);
    ADD_SYM_INST("ASHPI",  I_ASHP|I_CIS_I,  OC_CIS3);
    ADD_SYM_INST("CMPC",   I_CMPC,          OC_NONE);
    ADD_SYM_INST("CMPCI",  I_CMPC|I_CIS_I,  OC_CIS3);
    ADD_SYM_INST("CMPN",   I_CMPN,          OC_NONE);
    ADD_SYM_INST("CMPNI",  I_CMPN|I_CIS_I,  OC_CIS2);
    ADD_SYM_INST("CMPP",   I_CMPP,          OC_NONE);
    ADD_SYM_INST("CMPPI",  I_CMPP|I_CIS_I,  OC_CIS2);
    ADD_SYM_INST("CVTLN",  I_CVTLN,         OC_NONE);
    ADD_SYM_INST("CVTLNI", I_CVTLN|I_CIS_I, OC_CIS2);
    ADD_SYM_INST("CVTLP",  I_CVTPL,         OC_NONE);
    ADD_SYM_INST("CVTLPI", I_CVTLP|I_CIS_I, OC_CIS2);
    ADD_SYM_INST("CVTNL",  I_CVTNL,         OC_NONE);
    ADD_SYM_INST("CVTNLI", I_CVTNL|I_CIS_I, OC_CIS2);
    ADD_SYM_INST("CVTNP",  I_CVTNP,         OC_NONE);
    ADD_SYM_INST("CVTNPI", I_CVTNP|I_CIS_I, OC_CIS2);
    ADD_SYM_INST("CVTPL",  I_CVTPL,         OC_NONE);
    ADD_SYM_INST("CVTPLI", I_CVTPL|I_CIS_I, OC_CIS2);
    ADD_SYM_INST("CVTPN",  I_CVTPN,         OC_NONE);
    ADD_SYM_INST("CVTPNI", I_CVTPN|I_CIS_I, OC_CIS2);
    ADD_SYM_INST("DIVP",   I_DIVP,          OC_NONE);
    ADD_SYM_INST("DIVPI",  I_DIVP|I_CIS_I,  OC_CIS3);
    ADD_SYM_INST("L2D0",   I_L2Dr+0,        OC_NONE);
    ADD_SYM_INST("L2D1",   I_L2Dr+1,        OC_NONE);
    ADD_SYM_INST("L2D2",   I_L2Dr+2,        OC_NONE);
    ADD_SYM_INST("L2D3",   I_L2Dr+3,        OC_NONE);
    ADD_SYM_INST("L2D4",   I_L2Dr+4,        OC_NONE);
    ADD_SYM_INST("L2D5",   I_L2Dr+5,        OC_NONE);
    ADD_SYM_INST("L2D6",   I_L2Dr+6,        OC_NONE);
    ADD_SYM_INST("L2D7",   I_L2Dr+7,        OC_NONE);
    ADD_SYM_INST("L3D0",   I_L3Dr+0,        OC_NONE);
    ADD_SYM_INST("L3D1",   I_L3Dr+1,        OC_NONE);
    ADD_SYM_INST("L3D2",   I_L3Dr+2,        OC_NONE);
    ADD_SYM_INST("L3D3",   I_L3Dr+3,        OC_NONE);
    ADD_SYM_INST("L3D4",   I_L3Dr+4,        OC_NONE);
    ADD_SYM_INST("L3D5",   I_L3Dr+5,        OC_NONE);
    ADD_SYM_INST("L3D6",   I_L3Dr+6,        OC_NONE);
    ADD_SYM_INST("L3D7",   I_L3Dr+7,        OC_NONE);
    ADD_SYM_INST("LOCC",   I_LOCC,          OC_NONE);
    ADD_SYM_INST("LOCCI",  I_LOCC|I_CIS_I,  OC_CIS2);
    ADD_SYM_INST("MATC",   I_MATC,          OC_NONE);
    ADD_SYM_INST("MATCI",  I_MATC|I_CIS_I,  OC_CIS2);
    ADD_SYM_INST("MOVC",   I_MOVC,          OC_NONE);
    ADD_SYM_INST("MOVCI",  I_MOVC|I_CIS_I,  OC_CIS3);
    ADD_SYM_INST("MOVRC",  I_MOVRC,         OC_NONE);
    ADD_SYM_INST("MOVRCI", I_MOVRC|I_CIS_I, OC_CIS3);
    ADD_SYM_INST("MOVTC",  I_MOVTC,         OC_NONE);
    ADD_SYM_INST("MOVTCI", I_MOVTC|I_CIS_I, OC_CIS4);
    ADD_SYM_INST("MULP",   I_MULP,          OC_NONE);
    ADD_SYM_INST("MULPI",  I_MULP|I_CIS_I,  OC_CIS3);
    ADD_SYM_INST("SCANC",  I_SCANC,         OC_NONE);
    ADD_SYM_INST("SCANCI", I_SCANC|I_CIS_I, OC_CIS2);
    ADD_SYM_INST("SKPC",   I_SKPC,          OC_NONE);
    ADD_SYM_INST("SKPCI",  I_SKPC|I_CIS_I,  OC_CIS2);
    ADD_SYM_INST("SPANC",  I_SPANC,         OC_NONE);
    ADD_SYM_INST("SPANCI", I_SPANC|I_CIS_I, OC_CIS2);
    ADD_SYM_INST("SUBN",   I_SUBN,          OC_NONE);
    ADD_SYM_INST("SUBNI",  I_SUBN|I_CIS_I,  OC_CIS3);
    ADD_SYM_INST("SUBP",   I_SUBP,          OC_NONE);
    ADD_SYM_INST("SUBPI",  I_SUBP|I_CIS_I,  OC_CIS3);

#undef ADD_SYM_INST

/**********************************************************************/

    /* TODO: Why are we doing this (it seems to work without) */

    if (!STRINGENT)
        add_sym(current_section->label, 0,  
             /* 0, // This was the original flags value */
             /* (SYMBOLFLAG_PERMANENT | SYMBOLFLAG_DEFINITION), */
                SYMBOLFLAG_UNDEFINED,
                current_section, &section_st, NULL);
}

/* sym_hist is a diagnostic function that prints a histogram of the
   hash table useage of a symbol table.  I used this to try to tune
   the hash function for better spread.  It's not used now. */

void sym_hist(
    SYMBOL_TABLE *st,
    char *name)
{
    int             i;
    SYMBOL         *sym;

    fprintf(lstfile, "Histogram for symbol table %s\n", name);
    for (i = 0; i < 1023; i++) {
        fprintf(lstfile, "%4d: ", i);
        for (sym = st->hash[i]; sym != NULL; sym = sym->next)
            fputc('#', lstfile);
        fputc('\n', lstfile);
    }
}

/* rad50order returns the order of a RAD50 character */
/* Control-characters return lower than RAD50 and ...
 * ... non-RAD50 characters return higher than RAD50 */

static int rad50order(
    int c)
{
    enum rad50value {
        R50_SPACE  = 000,  /* ' ' =  0. */
        R50_ALPHA  = 001,  /* 'A' =  1. */
                           /*  :        */
                           /* 'Z' = 26. */
        R50_DOLLAR = 033,  /* '$' = 27. */
        R50_DOT    = 034,  /* '.' = 28. */
        R50_UL     = 035,  /* '_' = 29. */
        R50_DIGIT  = 036,  /* '0' = 30. */
                           /*  :        */
                           /* '9' = 39. */
        R50_NOTR50 = 050   /* ... = 40. */
    };

    if (isupper(c))
        return R50_ALPHA;

    if (isdigit(c))
        return R50_DIGIT;

    switch (c) {
    case ' ':
        return R50_SPACE;

    case '$':
        return R50_DOLLAR;

    case '.':
        return R50_DOT;

    case '_':
        return R50_UL;
    }

    if (!iscntrl(c))
        return R50_NOTR50;

    return -1;
}

/* rad50cmp has the same function as strcmp() but with the RAD50 sort order. */
/* The parameters are expected to contain only characters from the RAD50 set.
 * The sort-order is for the first non-matching character in the two strings.
 * '\nnn' sorts lowest and non-RAD50 characters (including lower-case) sort highest. */

static int rad50cmp(
    const char *p1,
    const char *p2)
{
    const unsigned char *s1 = (const unsigned char *) p1;
    const unsigned char *s2 = (const unsigned char *) p2;
          unsigned char  c1,
                         c2;

    do {
        c1 = *s1++;
        c2 = *s2++;
        if (c1 == '\0')
	    return -c2;
    } while (c1 == c2);

    {
       int r1 = rad50order((int) c1);
       int r2 = rad50order((int) c2);

       if (r1 != r2)
           return r1 - r2;
    }
    return c1 - c2;
}

#define rad50cmp strcmp  // Uncomment this line to disable sorting in RADIX50 order

int symbol_compar(
    const void *a,
    const void *b)
{
    SYMBOL *sa = *(SYMBOL **)a;
    SYMBOL *sb = *(SYMBOL **)b;

    return rad50cmp(sa->label, sb->label);
}

void list_symbol_table(
    void)
{
    SYMBOL_ITER iter;
    SYMBOL *sym;
    int skip_locals = 0;
    int longest_symbol = 6;
    int nsyms = 0;

    fprintf(lstfile,"\n\nSymbol table\n\n");

    /* Count the symbols in the table */
    for (sym = first_sym(&symbol_st, &iter); sym != NULL; sym = next_sym(&symbol_st, &iter)) {
        if (skip_locals && sym->flags & SYMBOLFLAG_LOCAL) {
            continue;
        }
        nsyms++;
        { /**/
        int len = strlen(sym->label);

        if (len > longest_symbol) {
            longest_symbol = len;
        }
        } /**/
    }

    /* Sort them by name */
    { /**/
    { /**/
    SYMBOL **symbols = malloc(nsyms * sizeof (SYMBOL *));
    SYMBOL **symbolp = symbols;

    for (sym = first_sym(&symbol_st, &iter); sym != NULL; sym = next_sym(&symbol_st, &iter)) {
        if (skip_locals && sym->flags & SYMBOLFLAG_LOCAL) {
            continue;
        }
        *symbolp++ = sym;
    }

    qsort(symbols, nsyms, sizeof(SYMBOL *), symbol_compar);

    symbolp = symbols;

    /* Print the listing in NCOLS columns. */
    { /**/
    int ncols = (132 / (longest_symbol + 19));
    int nlines = (nsyms + ncols - 1) / ncols;
    int line;
    /*
     * DIRER$  =%004562RGX    006
     * ^        ^^     ^      ^-- for R symbols: program segment number
     * |        ||     +-- Flags: R = relocatable
     * |        ||                G = global
     * |        ||                X = implicit global
     * |        ||                L = local
     * |        ||                W = weak
     * |        |+- value, ****** for if it was not a definition
     * |        +-- % for a register
     * +- label name
     */

    for (line = 0; line < nlines; line++) {
        int i;

        for (i = line; i < nsyms; i += nlines) {
            sym = symbols[i];
            check_sym_invariants(sym, __FILE__, __LINE__, NULL);

            fprintf(lstfile,"%-*s", longest_symbol, sym->label);
            fprintf(lstfile,"%c", (sym->section->flags & PSECT_REL) ? ' ' : '=');
            fprintf(lstfile,"%c", (sym->section->type == SECTION_REGISTER) ? '%' : ' ');
            if (!(sym->flags & SYMBOLFLAG_DEFINITION)) {
                fprintf(lstfile,"******");
            } else {
                fprintf(lstfile,"%06o", sym->value & 0177777);
            }
            fprintf(lstfile,"%c", (sym->section->flags & PSECT_REL) ? 'R' : ' ');
            fprintf(lstfile,"%c", (sym->flags & SYMBOLFLAG_GLOBAL) ?  'G' : ' ');
            fprintf(lstfile,"%c", (sym->flags & SYMBOLFLAG_IMPLICIT_GLOBAL) ? 'X' : ' ');
            fprintf(lstfile,"%c", (sym->flags & SYMBOLFLAG_LOCAL) ?   'L' : ' ');
            fprintf(lstfile,"%c", (sym->flags & SYMBOLFLAG_WEAK) ?    'W' : ' ');
            if (sym->section->sector != 0) {
                fprintf(lstfile,"  %03d ", sym->section->sector);
            } else {
                fprintf(lstfile,"      ");
            }
        }
        fprintf(lstfile,"\n");
    }
    } /**/

    /* List sections */

    fprintf(lstfile,"\n\nProgram sections" /* ":" */ "\n\n");

    { /**/
    int i;

    for (i = 0; i < sector; i++) {
        list_section(sections[i]);
    }
    } /**/

    free(symbols);
    } /**/
    } /**/
}

void list_section(
    SECTION *sec)
{
    if (sec == NULL) {
        fprintf(lstfile, "(null)\n");
        return;
    }

    { /**/
    int flags = sec->flags;

    fprintf(lstfile, "%-6s  %06o    %03d   ",
        sec->label, sec->size & 0177777, sec->sector);
    fprintf(lstfile, "(%s,%s,%s,%s,%s,%s)\n",
        (flags & PSECT_RO)   ? "RO"  : "RW",
        (flags & PSECT_DATA) ? "D"   : "I",
        (flags & PSECT_GBL)  ? "GBL" : "LCL",
        (flags & PSECT_REL)  ? "REL" : "ABS",
        (flags & PSECT_COM)  ? "OVR" : "CON",
        (flags & PSECT_SAV)  ? "SAV" : "NOSAV");
    } /**/
}
