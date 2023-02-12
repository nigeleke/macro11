
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

SYMBOL         *reg_sym[8];     /* Keep the register symbols in a handy array */


SYMBOL_TABLE    system_st;      /* System symbols (Instructions,
                                   pseudo-ops, registers) */

SYMBOL_TABLE    section_st;     /* Program sections */

SYMBOL_TABLE    symbol_st;      /* User symbols */

SYMBOL_TABLE    macro_st;       /* Macros */

SYMBOL_TABLE    implicit_st;    /* The symbols which may be implicit globals */

SYMBOL_TABLE    undefined_st;   /* The symbols which may be undefined */


void list_section(SECTION *sec);

static void
dump_sym(SYMBOL *sym)
{
    report(NULL, "'%s': %06o, stmt %d, flags %o:%s%s%s%s%s%s\n",
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

void
check_sym_invariants(SYMBOL *sym, char *file, int line, STREAM *stream)
{
    int dump = 0;

    if (sym->section == &instruction_section) {
        /* The instructions use the flags field differently */
        if ((sym->flags & ~OC_MASK) != 0) {
            report(stream, "%s %d: Instruction symbol %s has wrong flags\n", file, line, sym->label);
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
            report(stream, "%s %d: Symbol '%s' definedness is inconsistent\n", file, line, sym->label);
            dump++;
    }

    if ( (sym->flags & SYMBOLFLAG_IMPLICIT_GLOBAL) &&
        !(sym->flags & SYMBOLFLAG_GLOBAL)) {
        report(stream, "%s %d: Symbol '%s' globalness is inconsistent\n", file, line, sym->label);
        dump++;
    }

    if ( (sym->flags & SYMBOLFLAG_LOCAL) &&
         (sym->flags & SYMBOLFLAG_GLOBAL)) {
        report(stream, "%s %d: Symbol '%s' is local and global\n", file, line, sym->label);
        dump++;
    }

    if ( (sym->flags & SYMBOLFLAG_PERMANENT) &&
        !(sym->flags & SYMBOLFLAG_DEFINITION)) {
        report(stream, "%s %d: Symbol '%s' is permanent without definition\n", file, line, sym->label);
        dump++;
    }

    if ( (sym->flags & SYMBOLFLAG_WEAK) &&
        !(sym->flags & SYMBOLFLAG_GLOBAL)) {
        report(stream, "%s %d: Symbol '%s' weak/global is inconsistent\n", file, line, sym->label);
        dump++;
    }

    if (dump) {
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
    char            label[SYMMAX_MAX + 1];      // big size

    if (isdigit((unsigned char)labelraw[0])) {
        // Don't truncate local labels
        strncpy(label, labelraw, SYMMAX_MAX);
        label[SYMMAX_MAX] = 0;
    } else {
        //JH: truncate symbol to SYMMAX
        strncpy(label, labelraw, symbol_len);
        label[symbol_len] = 0;
    }

    sym = lookup_sym(label, table);
    if (sym != NULL) {
        // A symbol registered as "undefined" can be changed.
        //
        check_sym_invariants(sym, __FILE__, __LINE__, stream);

        if (sym->flags & SYMBOLFLAG_PERMANENT) {
            if (flags & SYMBOLFLAG_PERMANENT) {
                if (sym->value != value || sym->section != section) {
                    report(stream, "Phase error: '%s'\n", label);
                    return NULL;
                }
            } else {
                report(stream, "Redefining permanent symbol '%s'\n", label);
                return NULL;
            }
        }

        if ((sym->flags & SYMBOLFLAG_UNDEFINED) && !(flags & SYMBOLFLAG_UNDEFINED)) {
            sym->flags &= ~(SYMBOLFLAG_PERMANENT | SYMBOLFLAG_UNDEFINED);
        }
        else if (!(sym->flags & SYMBOLFLAG_UNDEFINED) && (flags & SYMBOLFLAG_UNDEFINED)) {
            report(stream, "INTERNAL ERROR: Turning defined symbol '%s' into undefined\n", label);
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

        report(stream, "INTERNAL ERROR: Bad symbol '%s' redefinition\n", label);
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
#define ADD_SYM(a,b,c,d,e) add_sym(a,b,c,d,e,NULL)

    current_pc = ADD_SYM(".", 0, SYMBOLFLAG_DEFINITION, current_section, &symbol_st);

#define S (SYMBOLFLAG_PERMANENT | SYMBOLFLAG_DEFINITION)

    reg_sym[0] = ADD_SYM("R0", 0, S, &register_section, &system_st);
    reg_sym[1] = ADD_SYM("R1", 1, S, &register_section, &system_st);
    reg_sym[2] = ADD_SYM("R2", 2, S, &register_section, &system_st);
    reg_sym[3] = ADD_SYM("R3", 3, S, &register_section, &system_st);
    reg_sym[4] = ADD_SYM("R4", 4, S, &register_section, &system_st);
    reg_sym[5] = ADD_SYM("R5", 5, S, &register_section, &system_st);
    reg_sym[6] = ADD_SYM("SP", 6, S, &register_section, &system_st);
    reg_sym[7] = ADD_SYM("PC", 7, S, &register_section, &system_st);

    //JH: symbols longer than current SYMMAX will be truncated. SYMMAX=6 is minimum!

    ADD_SYM(".ASCII", P_ASCII, S, &pseudo_section, &system_st);
    ADD_SYM(".ASCIZ", P_ASCIZ, S, &pseudo_section, &system_st);
    ADD_SYM(".ASECT", P_ASECT, S, &pseudo_section, &system_st);
    ADD_SYM(".BLKB", P_BLKB, S, &pseudo_section, &system_st);
    ADD_SYM(".BLKW", P_BLKW, S, &pseudo_section, &system_st);
    ADD_SYM(".BYTE", P_BYTE, S, &pseudo_section, &system_st);
    ADD_SYM(".CSECT", P_CSECT, S, &pseudo_section, &system_st);
    ADD_SYM(".CROSS", P_CROSS, S, &pseudo_section, &system_st);
    ADD_SYM(".DSABL", P_DSABL, S, &pseudo_section, &system_st);
    ADD_SYM(".ENABL", P_ENABL, S, &pseudo_section, &system_st);
    ADD_SYM(".END", P_END, S, &pseudo_section, &system_st);
    ADD_SYM(".ENDC", P_ENDC, S, &pseudo_section, &system_st);
    ADD_SYM(".ENDM", P_ENDM, S, &pseudo_section, &system_st);
    ADD_SYM(".ENDR", P_ENDR, S, &pseudo_section, &system_st);
    ADD_SYM(".EOT", P_EOT, S, &pseudo_section, &system_st);
    ADD_SYM(".ERROR", P_ERROR, S, &pseudo_section, &system_st);
    ADD_SYM(".EVEN", P_EVEN, S, &pseudo_section, &system_st);
    ADD_SYM(".FLT2", P_FLT2, S, &pseudo_section, &system_st);
    ADD_SYM(".FLT4", P_FLT4, S, &pseudo_section, &system_st);
    ADD_SYM(".GLOBL", P_GLOBL, S, &pseudo_section, &system_st);
    ADD_SYM(".IDENT", P_IDENT, S, &pseudo_section, &system_st);
    ADD_SYM(".IF", P_IF, S, &pseudo_section, &system_st);
    ADD_SYM(".IFDF", P_IFDF, S, &pseudo_section, &system_st);
    ADD_SYM(".IFNDF", P_IFDF, S, &pseudo_section, &system_st);
    ADD_SYM(".IFF", P_IFF, S, &pseudo_section, &system_st);
    ADD_SYM(".IFT", P_IFT, S, &pseudo_section, &system_st);
    ADD_SYM(".IFTF", P_IFTF, S, &pseudo_section, &system_st);
    ADD_SYM(".IIF", P_IIF, S, &pseudo_section, &system_st);
    ADD_SYM(".INCLUDE", P_INCLUDE, S, &pseudo_section, &system_st);
    ADD_SYM(".IRP", P_IRP, S, &pseudo_section, &system_st);
    ADD_SYM(".IRPC", P_IRPC, S, &pseudo_section, &system_st);
    ADD_SYM(".LIBRARY", P_LIBRARY, S, &pseudo_section, &system_st);
    ADD_SYM(".LIMIT", P_LIMIT, S, &pseudo_section, &system_st);
    ADD_SYM(".LIST", P_LIST, S, &pseudo_section, &system_st);
    ADD_SYM(".MCALL", P_MCALL, S, &pseudo_section, &system_st);
    ADD_SYM(".MDELE", P_MDELETE, S, &pseudo_section, &system_st);
    ADD_SYM(".MEXIT", P_MEXIT, S, &pseudo_section, &system_st);
    ADD_SYM(".NARG", P_NARG, S, &pseudo_section, &system_st);
    ADD_SYM(".NCHR", P_NCHR, S, &pseudo_section, &system_st);
    ADD_SYM(".NLIST", P_NLIST, S, &pseudo_section, &system_st);
    ADD_SYM(".NOCRO", P_NOCROSS, S, &pseudo_section, &system_st);
    ADD_SYM(".NTYPE", P_NTYPE, S, &pseudo_section, &system_st);
    ADD_SYM(".ODD", P_ODD, S, &pseudo_section, &system_st);
    ADD_SYM(".PACKED", P_PACKED, S, &pseudo_section, &system_st);
    ADD_SYM(".PAGE", P_PAGE, S, &pseudo_section, &system_st);
    ADD_SYM(".PRINT", P_PRINT, S, &pseudo_section, &system_st);
    ADD_SYM(".PSECT", P_PSECT, S, &pseudo_section, &system_st);
    ADD_SYM(".RADIX", P_RADIX, S, &pseudo_section, &system_st);
    ADD_SYM(".RAD50", P_RAD50, S, &pseudo_section, &system_st);
    ADD_SYM(".REM", P_REM, S, &pseudo_section, &system_st);
    ADD_SYM(".REPT", P_REPT, S, &pseudo_section, &system_st);
    ADD_SYM(".RESTORE", P_RESTORE, S, &pseudo_section, &system_st);
    ADD_SYM(".SAVE", P_SAVE, S, &pseudo_section, &system_st);
    ADD_SYM(".SBTTL", P_SBTTL, S, &pseudo_section, &system_st);
    ADD_SYM(".TITLE", P_TITLE, S, &pseudo_section, &system_st);
    ADD_SYM(".WORD", P_WORD, S, &pseudo_section, &system_st);
    ADD_SYM(".MACRO", P_MACRO, S, &pseudo_section, &system_st);
    ADD_SYM(".WEAK", P_WEAK, S, &pseudo_section, &system_st);

#undef S

    ADD_SYM("ADC", I_ADC, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("ADCB", I_ADCB, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("ADD", I_ADD, OC_2GEN, &instruction_section, &system_st);
    ADD_SYM("ASH", I_ASH, OC_ASH, &instruction_section, &system_st);
    ADD_SYM("ASHC", I_ASHC, OC_ASH, &instruction_section, &system_st);
    ADD_SYM("ASL", I_ASL, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("ASLB", I_ASLB, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("ASR", I_ASR, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("ASRB", I_ASRB, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("BCC", I_BCC, OC_BR, &instruction_section, &system_st);
    ADD_SYM("BCS", I_BCS, OC_BR, &instruction_section, &system_st);
    ADD_SYM("BEQ", I_BEQ, OC_BR, &instruction_section, &system_st);
    ADD_SYM("BGE", I_BGE, OC_BR, &instruction_section, &system_st);
    ADD_SYM("BGT", I_BGT, OC_BR, &instruction_section, &system_st);
    ADD_SYM("BHI", I_BHI, OC_BR, &instruction_section, &system_st);
    ADD_SYM("BHIS", I_BHIS, OC_BR, &instruction_section, &system_st);
    ADD_SYM("BIC", I_BIC, OC_2GEN, &instruction_section, &system_st);
    ADD_SYM("BICB", I_BICB, OC_2GEN, &instruction_section, &system_st);
    ADD_SYM("BIS", I_BIS, OC_2GEN, &instruction_section, &system_st);
    ADD_SYM("BISB", I_BISB, OC_2GEN, &instruction_section, &system_st);
    ADD_SYM("BIT", I_BIT, OC_2GEN, &instruction_section, &system_st);
    ADD_SYM("BITB", I_BITB, OC_2GEN, &instruction_section, &system_st);
    ADD_SYM("BLE", I_BLE, OC_BR, &instruction_section, &system_st);
    ADD_SYM("BLO", I_BLO, OC_BR, &instruction_section, &system_st);
    ADD_SYM("BLOS", I_BLOS, OC_BR, &instruction_section, &system_st);
    ADD_SYM("BLT", I_BLT, OC_BR, &instruction_section, &system_st);
    ADD_SYM("BMI", I_BMI, OC_BR, &instruction_section, &system_st);
    ADD_SYM("BNE", I_BNE, OC_BR, &instruction_section, &system_st);
    ADD_SYM("BPL", I_BPL, OC_BR, &instruction_section, &system_st);
    ADD_SYM("BPT", I_BPT, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("BR", I_BR, OC_BR, &instruction_section, &system_st);
    ADD_SYM("BVC", I_BVC, OC_BR, &instruction_section, &system_st);
    ADD_SYM("BVS", I_BVS, OC_BR, &instruction_section, &system_st);
    ADD_SYM("CALL", I_CALL, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("CALLR", I_CALLR, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("CCC", I_CCC, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("CLC", I_CLC, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("CLN", I_CLN, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("CLR", I_CLR, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("CLRB", I_CLRB, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("CLV", I_CLV, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("CLZ", I_CLZ, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("CMP", I_CMP, OC_2GEN, &instruction_section, &system_st);
    ADD_SYM("CMPB", I_CMPB, OC_2GEN, &instruction_section, &system_st);
    ADD_SYM("COM", I_COM, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("COMB", I_COMB, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("DEC", I_DEC, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("DECB", I_DECB, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("DIV", I_DIV, OC_ASH, &instruction_section, &system_st);
    ADD_SYM("EMT", I_EMT, OC_MARK, &instruction_section, &system_st);
    ADD_SYM("FADD", I_FADD, OC_1REG, &instruction_section, &system_st);
    ADD_SYM("FDIV", I_FDIV, OC_1REG, &instruction_section, &system_st);
    ADD_SYM("FMUL", I_FMUL, OC_1REG, &instruction_section, &system_st);
    ADD_SYM("FSUB", I_FSUB, OC_1REG, &instruction_section, &system_st);
    ADD_SYM("HALT", I_HALT, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("INC", I_INC, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("INCB", I_INCB, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("IOT", I_IOT, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("JMP", I_JMP, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("JSR", I_JSR, OC_JSR, &instruction_section, &system_st);
    ADD_SYM("MARK", I_MARK, OC_MARK, &instruction_section, &system_st);
    ADD_SYM("MED6X", I_MED6X, OC_NONE, &instruction_section, &system_st); /* PDP-11/60 Maintenance */
    ADD_SYM("MED74C", I_MED74C, OC_NONE, &instruction_section, &system_st); /* PDP-11/74 CIS Maintenance */
    ADD_SYM("MFPD", I_MFPD, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("MFPI", I_MFPI, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("MFPS", I_MFPS, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("MOV", I_MOV, OC_2GEN, &instruction_section, &system_st);
    ADD_SYM("MOVB", I_MOVB, OC_2GEN, &instruction_section, &system_st);
    ADD_SYM("MTPD", I_MTPD, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("MTPI", I_MTPI, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("MTPS", I_MTPS, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("MUL", I_MUL, OC_ASH, &instruction_section, &system_st);
    ADD_SYM("NEG", I_NEG, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("NEGB", I_NEGB, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("NOP", I_NOP, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("RESET", I_RESET, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("RETURN", I_RETURN, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("ROL", I_ROL, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("ROLB", I_ROLB, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("ROR", I_ROR, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("RORB", I_RORB, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("RTI", I_RTI, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("RTS", I_RTS, OC_1REG, &instruction_section, &system_st);
    ADD_SYM("RTT", I_RTT, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("SBC", I_SBC, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("SBCB", I_SBCB, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("SCC", I_SCC, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("SEC", I_SEC, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("SEN", I_SEN, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("SEV", I_SEV, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("SEZ", I_SEZ, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("SOB", I_SOB, OC_SOB, &instruction_section, &system_st);
    ADD_SYM("SPL", I_SPL, OC_1REG, &instruction_section, &system_st);
    ADD_SYM("SUB", I_SUB, OC_2GEN, &instruction_section, &system_st);
    ADD_SYM("SWAB", I_SWAB, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("SXT", I_SXT, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("TRAP", I_TRAP, OC_MARK, &instruction_section, &system_st);
    ADD_SYM("TST", I_TST, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("TSTB", I_TSTB, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("WAIT", I_WAIT, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("XFC", I_XFC, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("XOR", I_XOR, OC_JSR, &instruction_section, &system_st);
    ADD_SYM("MFPT", I_MFPT, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("CSM", I_CSM, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("TSTSET", I_TSTSET, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("WRTLCK", I_WRTLCK, OC_1GEN, &instruction_section, &system_st);

    /* FPP instructions */
    ADD_SYM("ABSD", I_ABSD, OC_FPP_FDST, &instruction_section, &system_st);
    ADD_SYM("ABSF", I_ABSF, OC_FPP_FDST, &instruction_section, &system_st);
    ADD_SYM("ADDD", I_ADDD, OC_FPP_FSRCAC, &instruction_section, &system_st);
    ADD_SYM("ADDF", I_ADDF, OC_FPP_FSRCAC, &instruction_section, &system_st);
    ADD_SYM("CFCC", I_CFCC, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("CLRD", I_CLRD, OC_FPP_FDST, &instruction_section, &system_st);
    ADD_SYM("CLRF", I_CLRF, OC_FPP_FDST, &instruction_section, &system_st);
    ADD_SYM("CMPD", I_CMPD, OC_FPP_FSRCAC, &instruction_section, &system_st);
    ADD_SYM("CMPF", I_CMPF, OC_FPP_FSRCAC, &instruction_section, &system_st);
    ADD_SYM("DIVD", I_DIVD, OC_FPP_FSRCAC, &instruction_section, &system_st);
    ADD_SYM("DIVF", I_DIVF, OC_FPP_FSRCAC, &instruction_section, &system_st);
    ADD_SYM("LDCDF", I_LDCDF, OC_FPP_FSRCAC, &instruction_section, &system_st);
    ADD_SYM("LDCFD", I_LDCFD, OC_FPP_FSRCAC, &instruction_section, &system_st);
    ADD_SYM("LDCID", I_LDCID, OC_FPP_SRCAC, &instruction_section, &system_st);
    ADD_SYM("LDCIF", I_LDCIF, OC_FPP_SRCAC, &instruction_section, &system_st);
    ADD_SYM("LDCLD", I_LDCLD, OC_FPP_SRCAC, &instruction_section, &system_st);
    ADD_SYM("LDCLF", I_LDCLF, OC_FPP_SRCAC, &instruction_section, &system_st);
    ADD_SYM("LDD", I_LDD, OC_FPP_FSRCAC, &instruction_section, &system_st);
    ADD_SYM("LDEXP", I_LDEXP, OC_FPP_SRCAC, &instruction_section, &system_st);
    ADD_SYM("LDF", I_LDF, OC_FPP_FSRCAC, &instruction_section, &system_st);
    ADD_SYM("LDFPS", I_LDFPS, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("MODD", I_MODD, OC_FPP_FSRCAC, &instruction_section, &system_st);
    ADD_SYM("MODF", I_MODF, OC_FPP_FSRCAC, &instruction_section, &system_st);
    ADD_SYM("MULD", I_MULD, OC_FPP_FSRCAC, &instruction_section, &system_st);
    ADD_SYM("MULF", I_MULF, OC_FPP_FSRCAC, &instruction_section, &system_st);
    ADD_SYM("NEGD", I_NEGD, OC_FPP_FDST, &instruction_section, &system_st);
    ADD_SYM("NEGF", I_NEGF, OC_FPP_FDST, &instruction_section, &system_st);
    ADD_SYM("SETD", I_SETD, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("SETF", I_SETF, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("SETI", I_SETI, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("SETL", I_SETL, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("STA0", I_STA0, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("STB0", I_STB0, OC_NONE, &instruction_section, &system_st);
    ADD_SYM("STCDF", I_STCDF, OC_FPP_ACFDST, &instruction_section, &system_st);
    ADD_SYM("STCDI", I_STCDI, OC_FPP_ACFDST, &instruction_section, &system_st);
    ADD_SYM("STCDL", I_STCDL, OC_FPP_ACFDST, &instruction_section, &system_st);
    ADD_SYM("STCFD", I_STCFD, OC_FPP_ACFDST, &instruction_section, &system_st);
    ADD_SYM("STCFI", I_STCFI, OC_FPP_ACFDST, &instruction_section, &system_st);
    ADD_SYM("STCFL", I_STCFL, OC_FPP_ACFDST, &instruction_section, &system_st);
    ADD_SYM("STD", I_STD, OC_FPP_ACFDST, &instruction_section, &system_st);
    ADD_SYM("STEXP", I_STEXP, OC_FPP_ACDST, &instruction_section, &system_st);
    ADD_SYM("STF", I_STF, OC_FPP_ACFDST, &instruction_section, &system_st);
    ADD_SYM("STFPS", I_STFPS, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("STST", I_STST, OC_1GEN, &instruction_section, &system_st);
    ADD_SYM("SUBD", I_SUBD, OC_FPP_FSRCAC, &instruction_section, &system_st);
    ADD_SYM("SUBF", I_SUBF, OC_FPP_FSRCAC, &instruction_section, &system_st);
    ADD_SYM("TSTD", I_TSTD, OC_FPP_FDST, &instruction_section, &system_st);
    ADD_SYM("TSTF", I_TSTF, OC_FPP_FDST, &instruction_section, &system_st);

    /* The CIS instructions */
    ADD_SYM("ADDNI", I_ADDN|I_CIS_I, OC_CIS3, &instruction_section, &system_st);
    ADD_SYM("ADDN",  I_ADDN,         OC_NONE, &instruction_section, &system_st);
    ADD_SYM("ADDPI", I_ADDP|I_CIS_I, OC_CIS3, &instruction_section, &system_st);
    ADD_SYM("ADDP",  I_ADDP,         OC_NONE, &instruction_section, &system_st);
    ADD_SYM("ASHNI", I_ASHN|I_CIS_I, OC_CIS3, &instruction_section, &system_st);
    ADD_SYM("ASHN",  I_ASHN,         OC_NONE, &instruction_section, &system_st);
    ADD_SYM("ASHPI", I_ASHP|I_CIS_I, OC_CIS3, &instruction_section, &system_st);
    ADD_SYM("ASHP",  I_ASHP,         OC_NONE, &instruction_section, &system_st);
    ADD_SYM("CMPCI", I_CMPC|I_CIS_I, OC_CIS3, &instruction_section, &system_st);
    ADD_SYM("CMPC",  I_CMPC,         OC_NONE, &instruction_section, &system_st);
    ADD_SYM("CMPNI", I_CMPN|I_CIS_I, OC_CIS2, &instruction_section, &system_st);
    ADD_SYM("CMPN",  I_CMPN,         OC_NONE, &instruction_section, &system_st);
    ADD_SYM("CMPPI", I_CMPP|I_CIS_I, OC_CIS2, &instruction_section, &system_st);
    ADD_SYM("CMPP",  I_CMPP,         OC_NONE, &instruction_section, &system_st);
    ADD_SYM("CVTLNI",I_CVTLN|I_CIS_I,OC_CIS2, &instruction_section, &system_st);
    ADD_SYM("CVTLN", I_CVTLN,        OC_NONE, &instruction_section, &system_st);
    ADD_SYM("CVTLPI",I_CVTLP|I_CIS_I,OC_CIS2, &instruction_section, &system_st);
    ADD_SYM("CVTLP", I_CVTPL,        OC_NONE, &instruction_section, &system_st);
    ADD_SYM("CVTNLI",I_CVTNL|I_CIS_I,OC_CIS2, &instruction_section, &system_st);
    ADD_SYM("CVTNL", I_CVTNL,        OC_NONE, &instruction_section, &system_st);
    ADD_SYM("CVTPLI",I_CVTPL|I_CIS_I,OC_CIS2, &instruction_section, &system_st);
    ADD_SYM("CVTPL", I_CVTPL,        OC_NONE, &instruction_section, &system_st);
    ADD_SYM("CVTNPI",I_CVTNP|I_CIS_I,OC_CIS2, &instruction_section, &system_st);
    ADD_SYM("CVTNP", I_CVTNP,        OC_NONE, &instruction_section, &system_st);
    ADD_SYM("CVTPNI",I_CVTPN|I_CIS_I,OC_CIS2, &instruction_section, &system_st);
    ADD_SYM("CVTPN", I_CVTPN,        OC_NONE, &instruction_section, &system_st);
    ADD_SYM("DIVPI", I_DIVP|I_CIS_I, OC_CIS3, &instruction_section, &system_st);
    ADD_SYM("DIVP",  I_DIVP,         OC_NONE, &instruction_section, &system_st);
    ADD_SYM("LOCCI", I_LOCC|I_CIS_I, OC_CIS2, &instruction_section, &system_st);
    ADD_SYM("LOCC",  I_LOCC,         OC_NONE, &instruction_section, &system_st);
    ADD_SYM("L2D0",  I_L2Dr+0,       OC_NONE, &instruction_section, &system_st);
    ADD_SYM("L2D1",  I_L2Dr+1,       OC_NONE, &instruction_section, &system_st);
    ADD_SYM("L2D2",  I_L2Dr+2,       OC_NONE, &instruction_section, &system_st);
    ADD_SYM("L2D3",  I_L2Dr+3,       OC_NONE, &instruction_section, &system_st);
    ADD_SYM("L2D4",  I_L2Dr+4,       OC_NONE, &instruction_section, &system_st);
    ADD_SYM("L2D5",  I_L2Dr+5,       OC_NONE, &instruction_section, &system_st);
    ADD_SYM("L2D6",  I_L2Dr+6,       OC_NONE, &instruction_section, &system_st);
    ADD_SYM("L2D7",  I_L2Dr+7,       OC_NONE, &instruction_section, &system_st);
    ADD_SYM("L3D0",  I_L3Dr+0,       OC_NONE, &instruction_section, &system_st);
    ADD_SYM("L3D1",  I_L3Dr+1,       OC_NONE, &instruction_section, &system_st);
    ADD_SYM("L3D2",  I_L3Dr+2,       OC_NONE, &instruction_section, &system_st);
    ADD_SYM("L3D3",  I_L3Dr+3,       OC_NONE, &instruction_section, &system_st);
    ADD_SYM("L3D4",  I_L3Dr+4,       OC_NONE, &instruction_section, &system_st);
    ADD_SYM("L3D5",  I_L3Dr+5,       OC_NONE, &instruction_section, &system_st);
    ADD_SYM("L3D6",  I_L3Dr+6,       OC_NONE, &instruction_section, &system_st);
    ADD_SYM("L3D7",  I_L3Dr+7,       OC_NONE, &instruction_section, &system_st);
    ADD_SYM("MATCI", I_MATC|I_CIS_I, OC_CIS2, &instruction_section, &system_st);
    ADD_SYM("MATC",  I_MATC,         OC_NONE, &instruction_section, &system_st);
    ADD_SYM("MOVCI", I_MOVC|I_CIS_I, OC_CIS3, &instruction_section, &system_st);
    ADD_SYM("MOVC",  I_MOVC,         OC_NONE, &instruction_section, &system_st);
    ADD_SYM("MOVRCI",I_MOVRC|I_CIS_I,OC_CIS3, &instruction_section, &system_st);
    ADD_SYM("MOVRC", I_MOVRC,        OC_NONE, &instruction_section, &system_st);
    ADD_SYM("MOVTCI",I_MOVTC|I_CIS_I,OC_CIS4, &instruction_section, &system_st);
    ADD_SYM("MOVTC", I_MOVTC,        OC_NONE, &instruction_section, &system_st);
    ADD_SYM("MULPI", I_MULP|I_CIS_I, OC_CIS3, &instruction_section, &system_st);
    ADD_SYM("MULP",  I_MULP,         OC_NONE, &instruction_section, &system_st);
    ADD_SYM("SCANCI",I_SCANC|I_CIS_I,OC_CIS2, &instruction_section, &system_st);
    ADD_SYM("SCANC", I_SCANC,        OC_NONE, &instruction_section, &system_st);
    ADD_SYM("SKPCI", I_SKPC|I_CIS_I, OC_CIS2, &instruction_section, &system_st);
    ADD_SYM("SKPC",  I_SKPC,         OC_NONE, &instruction_section, &system_st);
    ADD_SYM("SPANCI",I_SPANC|I_CIS_I,OC_CIS2, &instruction_section, &system_st);
    ADD_SYM("SPANC", I_SPANC,        OC_NONE, &instruction_section, &system_st);
    ADD_SYM("SUBNI", I_SUBN|I_CIS_I, OC_CIS3, &instruction_section, &system_st);
    ADD_SYM("SUBN",  I_SUBN,         OC_NONE, &instruction_section, &system_st);
    ADD_SYM("SUBPI", I_SUBP|I_CIS_I, OC_CIS3, &instruction_section, &system_st);
    ADD_SYM("SUBP",  I_SUBP,         OC_NONE, &instruction_section, &system_st);

    ADD_SYM(current_section->label, 0, 0, current_section, &section_st);
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

static int symbol_compar(
    const void *a,
    const void *b)
{
    SYMBOL *sa = *(SYMBOL **)a;
    SYMBOL *sb = *(SYMBOL **)b;

    return strcmp(sa->label, sb->label);
}

void list_symbol_table(
    void)
{
    SYMBOL_ITER iter;
    SYMBOL *sym;
    int skip_locals = 0;
    int longest_symbol = symbol_len;

    fprintf(lstfile,"\n\nSymbol table\n\n");

    /* Count the symbols in the table */
    int nsyms = 0;
    for (sym = first_sym(&symbol_st, &iter); sym != NULL; sym = next_sym(&symbol_st, &iter)) {
        if (skip_locals && sym->flags & SYMBOLFLAG_LOCAL) {
            continue;
        }
        nsyms++;
        int len = strlen(sym->label);
        if (len > longest_symbol) {
            longest_symbol = len;
        }
    }

    /* Sort them by name */
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

    /* List sections */

    fprintf(lstfile,"\n\nProgram sections:\n\n");

    int i;
    for (i = 0; i < sector; i++) {
        list_section(sections[i]);
    }

    free(symbols);
}

void list_section(
    SECTION *sec)
{
    if (sec == NULL) {
        fprintf(lstfile, "(null)\n");
        return;
    }

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
}
