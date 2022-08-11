#define ASSEMBLE__C


#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "assemble.h"                  /* my own definitions */

#include "assemble_globals.h"
#include "assemble_aux.h"

#include "util.h"

#include "mlb.h"
#include "object.h"
#include "listing.h"
#include "parse.h"
#include "symbols.h"
#include "extree.h"
#include "macros.h"
#include "rept_irpc.h"

#include "rad50.h"




#define CHECK_EOL       check_eol(stack, cp)

/* assemble a rad50 value (some number of words). */
static unsigned * assemble_rad50 (
    char *cp,
    int max,
    int *count,
    STACK *stack)
{
    char           *radstr;
    unsigned       *ret;
    int             i, len, wcnt;

    /*
     * Allocate storage sufficient for the rest of
     * the line.
     */
    radstr = memcheck(malloc(strlen(cp)));
    len = 0;

    do {
        cp = skipwhite(cp);
        if (*cp == '<') {
            EX_TREE        *value;
            /* A byte value */
            value = parse_unary_expr(cp, 0);
            cp = value->cp;
            if (value->type != EX_LIT) {
                report(stack->top, "Expression must be constant\n");
                radstr[len++] = 0;
            } else if (value->data.lit >= 050) {
                report(stack->top, "Invalid character value %o\n", value->data.lit);
                radstr[len++] = 0;
            } else {
                radstr[len++] = value->data.lit;
            }
            free_tree(value);
        } else {
            char            quote = *cp++;

            while (*cp && *cp != '\n' && *cp != quote) {
                int         ch = ascii2rad50(*cp++);

                if (ch == -1) {
                    report(stack->top, "Invalid character '%c'\n", cp[-1]);
                    radstr[len++] = 0;
                } else {
                    radstr[len++] = ch;
                }

            }
            cp++;  /* Skip closing quote */
        }

        cp = skipwhite(cp);
    } while (!EOL(*cp));

    wcnt = (len + 2) / 3;
    /* Return at most "max" words, if specified */
    if (max && max < wcnt)
        wcnt = max;
    if (count != NULL)
        *count = wcnt;

    /* Allocate space for actual or max words, whichever is larger */
    ret = memcheck (malloc (((wcnt < max) ? max : wcnt) * sizeof (int)));
    for (i = 0; i < wcnt; i++) {
        int word = packrad50word(radstr + i * 3, len - (i * 3));
        ret[i] = word;
    }
    /* If max is specified, zero fill */
    for (; i < max; i++)
        ret[i] = 0;

    free(radstr);
    return ret;
}

/* assemble - read a line from the input stack, assemble it. */

/* This function is way way too large, because I just coded most of
   the operation code and pseudo-op handling right in line.  */

static int assemble(
    STACK *stack,
    TEXT_RLD *tr)
{
    char           *cp;         /* Parse character pointer */
    char           *opcp;       /* Points to operation mnemonic text */
    char           *ncp;        /* "next" cp */
    char           *label;      /* A label */
    char           *line;       /* The whole line */
    SYMBOL         *op;         /* The operation SYMBOL */
    int             local;      /* Whether a label is a local label or
                                   not */

    line = stack_getline(stack);
    if (line == NULL)
        return -1;                     /* Return code for EOF. */

    if (!enabl_lc) {                   /* If lower case disabled, */
        upcase(line);                  /* turn it into upper case. */
    }

    cp = line;

    /* Frankly, I don't need to keep "line."  But I found it quite
       handy during debugging, to see what the whole operation was,
       when I'm down to parsing the second operand and things aren't
       going right. */

    stmtno++;                          /* Increment statement number */

    list_source(stack->top, line);     /* List source */

    if (suppressed) {
        /* Assembly is suppressed by unsatisfied conditional.  Look
           for ending and enabling statements. */

        op = get_op(cp, &cp);          /* Look at operation code */

        /* FIXME: this code will blindly look into .REM commentary and
           find operation codes.  Incidentally, so will read_body().

           That doesn't really matter, though, since the original also
           did that (line 72 ends the suppressed conditional block):

     69                                         .if NE,0
     70                                         .rem    &
     71                                 junk
     72                                         .endc
A    73 000144  000000G 000000G         more junk
A    74 000150  000000G 000000G 000000G line that ends the comments with &
        000156  000000G 000000G 000000C
O    75                                         .endc
         */

        if (op == NULL)
            return 1;                  /* Not found.  Don't care. */
        if (op->section->type != SECTION_PSEUDO)
            return 1;                  /* Not a pseudo-op. */
        switch (op->value) {
        case P_IF:
        case P_IFDF:
            suppressed++;              /* Nested.  Suppressed. */
            break;
        case P_IFTF:
            if (suppressed == 1)       /* Reduce suppression from 1 to 0. */
                suppressed = 0;
            break;
        case P_IFF:
            if (suppressed == 1) {     /* Can reduce suppression from 1 to 0. */
                if (!conds[last_cond].ok)
                    suppressed = 0;
            }
            break;
        case P_IFT:
            if (suppressed == 1) {     /* Can reduce suppression from 1 to 0. */
                if (conds[last_cond].ok)
                    suppressed = 0;
            }
            break;
        case P_ENDC:
            suppressed--;              /* Un-nested. */
            if (suppressed == 0)
                pop_cond(last_cond - 1);        /* Re-enabled. */
            break;
        }
        return 1;
    }

    /* The line may begin with "label<ws>:[:]" */

    /* PSEUDO P_IIF jumps here.  */
  reassemble:
    opcp = cp;
    if ((label = get_symbol(cp, &ncp, &local)) != NULL) {
        int             flag = SYMBOLFLAG_PERMANENT | SYMBOLFLAG_DEFINITION | local;
        SYMBOL         *sym;

        ncp = skipwhite(ncp);
        if (*ncp == ':') {             /* Colon, for symbol definition? */
            ncp++;
            /* maybe it's a global definition */
            if (*ncp == ':') {
                flag |= SYMBOLFLAG_GLOBAL;      /* Yes, include global flag */
                ncp++;
            }

            sym = add_sym(label, DOT, flag, current_pc->section, &symbol_st,
                          stack->top);
            cp = ncp;

            if (sym == NULL)
                report(stack->top, "Invalid symbol definition '%s'\n", label);

            free(label);

            /* See if local symbol block should be incremented */
            if (!enabl_lsb && !local) {
                lsb = get_next_lsb();
            }

            cp = skipwhite(ncp);
            opcp = cp;
            label = get_symbol(cp, &ncp, NULL); /* Now, get what follows */
        }
    }

    cp = skipwhite(cp);

    if (EOL(*cp))
        return 1;                      /* It's commentary.  All done. */

    if (label) {                       /* Something looks like a label. */
        /* detect assignment */

        ncp = skipwhite(ncp);          /* The pointer to the text that
                                          follows the symbol */

        if (*ncp == '=') {
            unsigned        flags;
            EX_TREE        *value;
            SYMBOL         *sym;

            cp = ncp;

            /* Symbol assignment. */

            flags = SYMBOLFLAG_DEFINITION | local;
            cp++;
            if (*cp == '=') {
                flags |= SYMBOLFLAG_GLOBAL;     /* Global definition */
                cp++;
            }
            if (*cp == ':') {
                flags |= SYMBOLFLAG_PERMANENT;
                cp++;
            }

            cp = skipwhite(cp);

            value = parse_expr(cp, 0);
            cp = value->cp;

            /* Special code: if the symbol is the program counter,
               this is harder. */

            if (strcmp(label, ".") == 0) {
                if (current_pc->section->flags & PSECT_REL) {
                    SYMBOL         *symb;
                    unsigned        offset;

                    /* Express the given expression as a symbol and an
                       offset. The symbol must not be global, the
                       section must = current. */

                    if (!express_sym_offset(value, &symb, &offset)) {
                        report(stack->top, "Invalid ORG (for relocatable section)\n");
                    } else if (SYM_IS_IMPORTED(symb)) {
                        report(stack->top, "Can't ORG to external location\n");
                    } else if (symb->flags & SYMBOLFLAG_UNDEFINED) {
                        report(stack->top, "Can't ORG to undefined sym\n");
                    } else if (symb->section != current_pc->section) {
                        report(stack->top, "Can't ORG to alternate section (use PSECT)\n");
                    } else {
                        DOT = symb->value + offset;
                        list_value(stack->top, DOT);
                        change_dot(tr, 0);
                    }
                } else {
                    /* If the current section is absolute, the value
                       must be a literal */
                    if (value->type != EX_LIT) {
                        report(stack->top, "Can't ORG to non-absolute location\n");
                        free_tree(value);
                        free(label);
                        return 0;
                    }
                    DOT = value->data.lit;
                    list_value(stack->top, DOT);
                    change_dot(tr, 0);
                }
                free_tree(value);
                free(label);
                return CHECK_EOL;
            }

            /* regular symbols */
            if (value->type == EX_LIT) {
                sym = add_sym(label, value->data.lit, flags, &absolute_section,
                              &symbol_st, stack->top);
            } else if (value->type == EX_SYM || value->type == EX_TEMP_SYM) {
                sym = add_sym(label, value->data.symbol->value, flags,
                              value->data.symbol->section, &symbol_st, stack->top);
            } else {
                report(stack->top, "Complex expression cannot be assigned to a symbol\n");

                if (!pass) {
                    /* This may work better in pass 2 - something in
                       RT-11 monitor needs the symbol to apear to be
                       defined even if I can't resolve its value. */
                    sym = add_sym(label, 0, SYMBOLFLAG_UNDEFINED,
                                  &absolute_section, &symbol_st, stack->top);
                } else
                    sym = NULL;
            }

            if (sym != NULL)
                list_value(stack->top, sym->value);

            free_tree(value);
            free(label);

            return sym != NULL && CHECK_EOL;
        }

        /* Try to resolve macro */

do_mcalled_macro:
        op = lookup_sym(label, &macro_st);
        if (op /*&& op->stmtno < stmtno*/) {
            STREAM         *macstr;

            free(label);

            list_location(stack->top, DOT);

            macstr = expandmacro(stack->top, (MACRO *) op, ncp);
            if (macstr == NULL) {
                /* Error in expanding the macro, stop now. */
                return 0;
            }

            stack_push(stack, macstr); /* Push macro expansion
                                          onto input stream */

            return 1;
        }

        /* Try to resolve instruction or pseudo */
        op = lookup_sym(label, &system_st);
        if (op) {
            cp = ncp;

            free(label);               /* Don't need this hanging around anymore */

            switch (op->section->type) {
            case SECTION_PSEUDO:
                switch (op->value) {
                case P_PAGE:
                case P_PRINT:
                case P_SBTTL:
                case P_CROSS:
                case P_NOCROSS:
                    return 1;          /* Accepted, ignored.  (An obvious
                                          need: get assembly listing
                                          controls working fully. ) */
                case P_LIST:
                    if (pass > 0) {
                        cp = skipwhite(cp);
                        if (EOL(*cp))
                            list_level++;
                    }
                    return 1;
                case P_NLIST:
                    if (pass > 0) {
                        cp = skipwhite(cp);
                        if (EOL(*cp))
                            list_level--;
                    }
                    return 1;

                case P_IDENT:
                    if (ident)          /* An existing ident? */
                        free(ident);    /* Discard it. */

                    ident = assemble_rad50 (cp, 2, NULL, stack);
                    return 1;

                case P_RADIX:
                    {
                        int             old_radix = radix;
                        EX_TREE        *value;
                        int             ok = 1;

                        cp = skipwhite(cp);
                        if (EOL(*cp)) {
                            /* If no argument, assume 8 */
                            radix = 8;
                            return 1;
                        }
                        /* Parse the argument in decimal radix */
                        radix = 10;
                        value = parse_expr(cp, 0);
                        cp = value->cp;

                        if (value->type != EX_LIT) {
                            report(stack->top, "Argument to .RADIX must be constant\n");
                            radix = old_radix;
                            ok = 0;
                        } else {
                            radix = value->data.lit;
                            list_value(stack->top, radix);
                            if (radix != 8 && radix != 10 &&
                                radix != 2 && radix != 16) {
                                radix = old_radix;
                                report(stack->top, "Argument to .RADIX must be 2, 8, 10, or 16\n");
                                ok = 0;
                            }
                        }
                        free_tree(value);
                        return ok && CHECK_EOL;
                    }

                case P_FLT4:
                case P_FLT2:
                    {
                        int             ok = 1;

                        while (ok && !EOL(*cp)) {
                            unsigned        flt[4];

                            if (parse_float(cp, &cp, (op->value == P_FLT4 ? 4 : 2), flt)) {
                                /* All is well */
                            } else {
                                report(stack->top, "Bad floating point format\n");
                                ok = 0;         /* Don't try to parse the rest of the line */
                                flt[0] = flt[1] /* Store zeroes */
                                       = flt[2]
                                       = flt[3] = 0;
                            }
                            /* Store the word values */
                            store_word(stack->top, tr, 2, flt[0]);
                            store_word(stack->top, tr, 2, flt[1]);
                            if (op->value == P_FLT4) {
                                store_word(stack->top, tr, 2, flt[2]);
                                store_word(stack->top, tr, 2, flt[3]);
                            }
                            cp = skipdelim(cp);
                        }
                        return ok && CHECK_EOL;
                    }

                case P_ERROR:
                    report(stack->top, "%.*s\n", strcspn(cp, "\n"), cp);
                    return 0;

                case P_SAVE:
                    if (sect_sp >= SECT_STACK_SIZE - 1) {
                        report(stack->top, "Too many saved sections for .SAVE\n");
                        return 0;
                    }
                    sect_sp++;
                    sect_stack[sect_sp] = current_pc->section;
                    dot_stack[sect_sp] = DOT;
                    return CHECK_EOL;

                case P_RESTORE:
                    if (sect_sp < 0) {
                        report(stack->top, "No saved section for .RESTORE\n");
                        return 0;
                    } else {
                        go_section(tr, sect_stack[sect_sp]);
                        DOT = dot_stack[sect_sp];
                        list_location(stack->top, DOT);
                        if (!enabl_lsb) {
                            lsb = get_next_lsb();
                        }
                        sect_sp--;
                    }
                    return CHECK_EOL;

                case P_NARG:
                    {
                        STREAM         *str;
                        MACRO_STREAM   *mstr;
                        int             islocal;

                        label = get_symbol(cp, &cp, &islocal);

                        if (label == NULL) {
                            report(stack->top, "Bad .NARG syntax (symbol expected)\n");
                            return 0;
                        }

                        /* Walk up the stream stack to find the
                           topmost macro stream */
                        for (str = stack->top; str != NULL && str->vtbl != &macro_stream_vtbl;
                             str = str->next) ;

                        if (!str) {
                            report(str, ".NARG not within macro expansion\n");
                            free(label);
                            return 0;
                        }

                        mstr = (MACRO_STREAM *) str;

                        add_sym(label, mstr->nargs, SYMBOLFLAG_DEFINITION | islocal,
                                &absolute_section, &symbol_st, stack->top);
                        free(label);
                        list_value(stack->top, mstr->nargs);
                        return CHECK_EOL;
                    }

                case P_NCHR:
                    {
                        char           *string;
                        int             islocal;

                        label = get_symbol(cp, &cp, &islocal);

                        if (label == NULL) {
                            report(stack->top, "Bad .NCHR syntax (symbol expected)\n");
                            return 0;
                        }

                        cp = skipdelim(cp);

                        string = getstring(cp, &cp);

                        add_sym(label, strlen(string), SYMBOLFLAG_DEFINITION | islocal,
                                &absolute_section, &symbol_st, stack->top);
                        free(label);
                        free(string);
                        return CHECK_EOL;
                    }

                case P_NTYPE:
                    {
                        ADDR_MODE       mode;
                        int             islocal;
                        char           *error;

                        label = get_symbol(cp, &cp, &islocal);
                        if (label == NULL) {
                            report(stack->top, "Bad .NTYPE syntax (symbol expected)\n");
                            return 0;
                        }

                        cp = skipdelim(cp);

                        if (!get_mode(cp, &cp, &mode, &error)) {
                            report(stack->top,
                                   "Bad .NTYPE addressing mode (%s)\n", error);
                            free(label);
                            return 0;
                        }

                        add_sym(label, mode.type, SYMBOLFLAG_DEFINITION | islocal,
                                &absolute_section, &symbol_st, stack->top);
                        free_addr_mode(&mode);
                        free(label);

                        return CHECK_EOL;
                    }

                case P_INCLUDE:
                    {
                        char           *name = getstring_fn(cp, &cp);
                        STREAM         *incl;
                        char            hitfile[FILENAME_MAX];

                        if (name == NULL) {
                            report(stack->top, "Bad .INCLUDE file name\n");
                            return 0;
                        }

                        name = defext (name, "MAC");
                        my_searchenv(name, "INCLUDE", hitfile, sizeof(hitfile));

                        if (hitfile[0] == '\0') {
                            report(stack->top, "Unable to find .INCLUDE file \"%s\"\n", name);
                            free(name);
                            return 0;
                        }

                        free(name);

                        incl = new_file_stream(hitfile);
                        if (incl == NULL) {
                            report(stack->top, "Unable to open .INCLUDE file \"%s\"\n", hitfile);
                            return 0;
                        }

                        stack_push(stack, incl);

                        return CHECK_EOL;
                    }

                case P_REM:
                    {
                        char            quote[4];

                        /* Read and discard lines until one with a
                           closing quote */

                        cp = skipwhite(cp);
                        quote[0] = *cp++;
                        quote[1] = '\n';
                        quote[2] = 0;

                        for (;;) {
                            cp += strcspn(cp, quote);
                            if (*cp == quote[0])
                                break; /* Found closing quote */
                            cp = stack_getline(stack);     /* Read next input line */
                            if (cp == NULL)
                                break; /* EOF */
                        }
                    }
                    return 1;

                case P_IRP:
                    {
                        STREAM         *str = expand_irp(stack, cp);

                        if (str)
                            stack_push(stack, str);
                        return str != NULL;
                    }

                case P_IRPC:
                    {
                        STREAM         *str = expand_irpc(stack, cp);

                        if (str)
                            stack_push(stack, str);
                        return str != NULL;
                    }

                case P_LIBRARY:
                    {
                        char            hitfile[FILENAME_MAX];
                        char           *name = getstring_fn(cp, &cp);

                        name = defext (name, "MLB");
                        my_searchenv(name, "MCALL", hitfile, sizeof(hitfile));

                        if (hitfile[0]) {
                            mlbs[nr_mlbs] = mlb_open(hitfile, 0);
                            if (mlbs[nr_mlbs] == NULL) {
                                report(stack->top, "Unable to register macro library \"%s\"\n", hitfile);
                            } else {
                                nr_mlbs++;
                            }
                        } else {
                            report(stack->top, "Unable to locate macro library \"%s\"\n", name);
                        }
                        free(name);
                    }
                    return CHECK_EOL;

                case P_MCALL:
                    {
                        for (;;) {
                            cp = skipdelim(cp);

                            if (EOL(*cp))
                                return 1;

                            /* (lib)macro syntax. Ignore (lib) for now. */
                            if (*cp == '(') {
                                char *close = strchr(cp + 1, ')');

                                if (close != NULL) {
                                    char *libname = cp + 1;
                                    (void)libname;
                                    *close = '\0';
                                    cp = close + 1;
                                }
                            }

                            label = get_symbol(cp, &cp, NULL);
                            if (!label) {
                                report(stack->top,
                                       "Invalid .MCALL syntax (symbol expected)\n");
                                return 0;
                            }

                            /* See if that macro's already defined */
                            if (lookup_sym(label, &macro_st)) {
                                free(label);    /* Macro already
                                                   registered.  No
                                                   prob. */
                                cp = skipdelim(cp);
                                continue;
                            }

                            /* Do the actual macro library search */
                            if (!do_mcall (label, stack))
                                report(stack->top, "MACRO '%s' not found\n", label);

                            free(label);
                        }
                        /* NOTREACHED */
                    }
                    return 1;

                case P_MACRO:
                    {
                        MACRO          *mac = defmacro(cp, stack, CALLED_NORMAL);

                        return mac != NULL;
                    }

                case P_MDELETE:
                    return 1;   /* TODO: or should it just be a NOP? */

                case P_MEXIT:
                    {
                        STREAM         *macstr;

                        /* Pop a stream from the input. */
                        /* It must be the first stream, and it must be */
                        /* a macro, rept, irp, or irpc. */
                        macstr = stack->top;
                        if (macstr->vtbl != &macro_stream_vtbl && macstr->vtbl != &rept_stream_vtbl
                            && macstr->vtbl != &irp_stream_vtbl && macstr->vtbl != &irpc_stream_vtbl) {
                            report(stack->top, ".MEXIT not within a macro\n");
                            return 0;
                        }

                        /* and finally, pop the macro */
                        stack_pop(stack);

                        return CHECK_EOL;
                    }

                case P_REPT:
                    {
                        STREAM         *reptstr = expand_rept(stack, cp);

                        if (reptstr)
                            stack_push(stack, reptstr);
                        return reptstr != NULL;
                    }

                case P_ENABL:
                    /* FIXME - add all the rest of the options. */
                    while (!EOL(*cp)) {
                        label = get_symbol(cp, &cp, NULL);
                        if (strcmp(label, "AMA") == 0)
                            enabl_ama = 1;
                        else if (strcmp(label, "LSB") == 0) {
                            enabl_lsb = 1;
                            lsb = get_next_lsb();
                        } else if (strcmp(label, "GBL") == 0) {
                            enabl_gbl = 1;
                        } else if (strcmp(label, "LC") == 0) {
                            enabl_lc = 1;
                        } else if (strcmp(label, "LCM") == 0) {
                            enabl_lcm = 1;
                        } else if (strcmp(label, "MCL") == 0) {
                            enabl_mcl = 1;
                        }
                        free(label);
                        cp = skipdelim(cp);
                    }
                    return 1;

                case P_DSABL:
                    /* FIXME Ditto as for .ENABL */
                    while (!EOL(*cp)) {
                        label = get_symbol(cp, &cp, NULL);
                        if (strcmp(label, "AMA") == 0)
                            enabl_ama = 0;
                        else if (strcmp(label, "LSB") == 0) {
                            lsb = get_next_lsb();
                            enabl_lsb = 0;
                        } else if (strcmp(label, "GBL") == 0) {
                            enabl_gbl = 0;
                        } else if (strcmp(label, "LC") == 0) {
                            enabl_lc = 0;
                        } else if (strcmp(label, "LCM") == 0) {
                            enabl_lcm = 0;
                        } else if (strcmp(label, "MCL") == 0) {
                            enabl_mcl = 0;
                        }
                        free(label);
                        cp = skipdelim(cp);
                    }
                    return 1;

                case P_LIMIT:
                    store_limits(stack->top, tr);
                    return CHECK_EOL;

                case P_TITLE:
                    /* accquire module name */
                    if (module_name != NULL) {
                        free(module_name);
                    }
                    module_name = get_symbol(cp, &cp, NULL);
                    return 1;

                case P_END:
                    /* Accquire transfer address */
                    cp = skipwhite(cp);
                    if (!EOL(*cp)) {
                        if (xfer_address)
                            free_tree(xfer_address);
                        xfer_address = parse_expr(cp, 0);
                        cp = xfer_address->cp;
                    }
                    return CHECK_EOL;

                case P_IFDF:
                    opcp = skipwhite(opcp);
                    cp = opcp + 3;     /* Point cp at the "DF" or
                                          "NDF" part */
                    /* FALLS THROUGH */
                case P_IIF:
                case P_IF:
                    {
                        EX_TREE        *value;
                        int             ok = FALSE;

                        label = get_symbol(cp, &cp, NULL);      /* Get condition */
                        cp = skipdelim(cp);

                        if (!label) {
                            report(stack->top, "Missing .(I)IF condition\n");
                        } else if (strcmp(label, "DF") == 0) {
                            value = parse_expr(cp, EVALUATE_DEFINEDNESS);
                            cp = value->cp;
                            ok = eval_defined(value);
                            free_tree(value);
                        } else if (strcmp(label, "NDF") == 0) {
                            value = parse_expr(cp, EVALUATE_DEFINEDNESS);
                            cp = value->cp;
                            ok = eval_undefined(value);
                            free_tree(value);
                        } else if (strcmp(label, "B") == 0 ||
                                   strcmp(label, "NB") == 0) {
                            /*
                             * Page 6-46 footnote 1 says
                             * "A macro argument (a form of symbolic argument)
                             * is enclosed within angle brackets or delimited
                             * by the circumflex construction, as described in
                             * section 7.3. For example,
                             *   <A,B,C>
                             *   ^/124/"
                             * but we don't enforce that here (yet) by using
                             * simply getstring().
                             */
                            cp = skipwhite(cp);
                            if (EOL(*cp)) {
                                ok = 1;
                            } else {
                                char           *thing, *end;

                                thing = getstring(cp, &cp);
                                end = skipwhite(thing);
                                ok = (*end == 0);
                                free(thing);
                            }
                            if (label[0] == 'N') {
                                ok = !ok;
                            }
                        } else if (strcmp(label, "IDN") == 0 ||
                                   strcmp(label, "DIF") == 0) {
                            char           *thing1,
                                           *thing2;

                            thing1 = getstring(cp, &cp);
                            cp = skipdelim(cp);
                            if (!EOL(*cp))
                                thing2 = getstring(cp, &cp);
                            else
                                thing2 = memcheck(strdup(""));

                            if (!enabl_lcm) {
                                upcase(thing1);
                                upcase(thing2);
                            }

                            ok = (strcmp(thing1, thing2) == 0);
                            if (label[0] == 'D') {
                                ok = !ok;
                            }
                            free(thing1);
                            free(thing2);
                        } else if (strcmp(label, "P1") == 0) {
                            ok = (pass == 0);
                        } else if (strcmp(label, "P2") == 0) {
                            ok = (pass == 1);
                        } else {
                            int             sword;
                            unsigned        uword;
                            EX_TREE        *tvalue = parse_expr(cp, 0);

                            cp = tvalue->cp;

                            if (tvalue->type != EX_LIT) {
                                report(stack->top, "Bad .IF expression\n");
                                list_value(stack->top, 0);
                                free_tree(tvalue);
                                ok = TRUE;     /* Pick something. */
                            } else {
                                unsigned        word;

                                /* Convert to signed and unsigned words */
                                sword = tvalue->data.lit & 0x7fff;

                                /* FIXME I don't know if the following
                                   is portable enough.  */
                                if (tvalue->data.lit & 0x8000)
                                    sword |= ~0x7FFF;   /* Render negative */

                                /* Reduce unsigned value to 16 bits */
                                uword = tvalue->data.lit & 0xffff;

                                if (strcmp(label, "EQ") == 0 || strcmp(label, "Z") == 0)
                                    ok = (uword == 0), word = uword;
                                else if (strcmp(label, "NE") == 0 || strcmp(label, "NZ") == 0)
                                    ok = (uword != 0), word = uword;
                                else if (strcmp(label, "GT") == 0 || strcmp(label, "G") == 0)
                                    ok = (sword > 0), word = sword;
                                else if (strcmp(label, "GE") == 0)
                                    ok = (sword >= 0), word = sword;
                                else if (strcmp(label, "LT") == 0 || strcmp(label, "L") == 0)
                                    ok = (sword < 0), word = sword;
                                else if (strcmp(label, "LE") == 0)
                                    ok = (sword <= 0), word = sword;
                                else
                                    ok = 0, word = 0;

                                list_value(stack->top, word);

                                free_tree(tvalue);
                            }
                        }

                        free(label);

                        if (op->value == P_IIF) {
                            stmtno++;  /* the second half is a
                                          separate statement */
                            if (ok) {
                                /* The "immediate if" */
                                /* Only slightly tricky. */
                                cp = skipdelim(cp);
                                goto reassemble;
                            }
                            return 1;           /* Ignore rest of line if
                                                   condition is false */
                        }

                        push_cond(ok, stack->top);

                        if (!ok)
                            suppressed++;       /* Assembly
                                                   suppressed
                                                   until .ENDC */
                    }
                    return CHECK_EOL;

                case P_IFF:
                    if (last_cond < 0) {
                        report(stack->top, "No conditional block active\n");
                        return 0;
                    }
                    if (conds[last_cond].ok)    /* Suppress if last cond
                                                   is true */
                        suppressed++;
                    return CHECK_EOL;

                case P_IFT:
                    if (last_cond < 0) {
                        report(stack->top, "No conditional block active\n");
                        return 0;
                    }
                    if (!conds[last_cond].ok)   /* Suppress if last cond
                                                   is false */
                        suppressed++;
                    return CHECK_EOL;

                case P_IFTF:
                    if (last_cond < 0) {
                        report(stack->top, "No conditional block active\n");
                        return 0;
                    }
                    return CHECK_EOL;           /* Don't suppress. */

                case P_ENDC:
                    if (last_cond < 0) {
                        report(stack->top, "No conditional block active\n");
                        return 0;
                    }

                    pop_cond(last_cond - 1);
                    return CHECK_EOL;

                case P_ENDM:
                    report(stack->top, "No macro definition block active\n");
                    return 0;

                case P_ENDR:
                    report(stack->top, "No repeat block active\n");
                    return 0;

                case P_EVEN:
                    cp = skipwhite(cp);
                    if (!EOL(*cp)) {
                        report(stack->top, ".EVEN must not have an argument\n");
                    }
                    if (DOT & 1) {
                        list_word(stack->top, DOT, 0, 1, "");
                        DOT++;
                        change_dot(tr, 0);
                    }
                    return 1;

                case P_ODD:
                    if (!EOL(*cp)) {
                        report(stack->top, ".ODD must not have an argument\n");
                    }
                    if (!(DOT & 1)) {
                        list_word(stack->top, DOT, 0, 1, "");
                        DOT++;
                        change_dot(tr, 0);
                    }
                    return 1;

                case P_ASECT:
                    if (!enabl_lsb) {
                        lsb = get_next_lsb();
                    }
                    go_section(tr, &absolute_section);
                    list_location(stack->top, DOT);
                    return CHECK_EOL;

                case P_CSECT:
                case P_PSECT:
                    {
                        SYMBOL         *sectsym;
                        SECTION        *sect;
                        unsigned int    old_flags = ~0u;

                        label = get_symbol(cp, &cp, NULL);
                        if (label == NULL) {
                            sect = &blank_section;
                        }
                        else {
                            sectsym = lookup_sym(label, &section_st);
                            if (sectsym) {
                                sect = sectsym->section;
                                free(label);
                                old_flags = sect->flags;
                            } else {
                                sect = new_section();
                                sect->label = label;
                                sect->flags = 0;
                                sect->pc = 0;
                                sect->size = 0;
                                sect->type = SECTION_USER;
                                sections[sector++] = sect;
                                sectsym = add_sym(label, 0,
                                        SYMBOLFLAG_DEFINITION, sect,
                                        &section_st, stack->top);

                                /* page 6-41 table 6-5 */
                                if (op->value == P_PSECT) {
                                    sect->flags |= PSECT_REL;
                                } else {
                                    sect->flags |= PSECT_REL | PSECT_COM | PSECT_GBL;
                                }
                            }
                        }

                        cp = skipdelim(cp);
                        if (!EOL(*cp)) {
                            while (cp = skipdelim(cp), !EOL(*cp)) {
                                /* Parse section options */
                                label = get_symbol(cp, &cp, NULL);
                                if (strcmp(label, "ABS") == 0) {
                                    sect->flags &= ~PSECT_REL;      /* Not relative */
                                    sect->flags |= PSECT_COM;       /* implies common */
                                } else if (strcmp(label, "REL") == 0) {
                                    sect->flags |= PSECT_REL;       /* Is relative */
                                } else if (strcmp(label, "SAV") == 0) {
                                    sect->flags |= PSECT_SAV;       /* Is root */
                                } else if (strcmp(label, "NOSAV") == 0) {
                                    sect->flags &= ~PSECT_SAV;      /* Is not root */
                                } else if (strcmp(label, "OVR") == 0) {
                                    sect->flags |= PSECT_COM;       /* Is common */
                                } else if (strcmp(label, "CON") == 0) {
                                    sect->flags &= ~PSECT_COM;      /* Concatenated */
                                } else if (strcmp(label, "RW") == 0) {
                                    sect->flags &= ~PSECT_RO;       /* Not read-only */
                                } else if (strcmp(label, "RO") == 0) {
                                    sect->flags |= PSECT_RO;        /* Is read-only */
                                } else if (strcmp(label, "I") == 0) {
                                    sect->flags &= ~PSECT_DATA;     /* Not data */
                                } else if (strcmp(label, "D") == 0) {
                                    sect->flags |= PSECT_DATA;      /* data */
                                } else if (strcmp(label, "GBL") == 0) {
                                    sect->flags |= PSECT_GBL;       /* Global */
                                } else if (strcmp(label, "LCL") == 0) {
                                    sect->flags &= ~PSECT_GBL;      /* Local */
                                } else {
                                    report(stack->top, "Unknown flag %s given to .PSECT directive\n", label);
                                    free(label);
                                    return 0;
                                }

                                free(label);
                            }
                            /* If a section is declared a second time, and flags
                             * are given, then they must be identical to the
                             * first time.
                             * See page 6-38 of AA-KX10A-TC_PDP-11_MACRO-11_Reference_Manual_May88.pdf .
                             */
                            if (old_flags != ~0u && sect->flags != old_flags) {
                                /* The manual also says that any different
                                 * flags are ignored, and an error issued.
                                 * Apparently, that isn't true.
                                 * Kermit seems to do this in k11cmd.mac:
                                 *      .psect  $pdata          ; line 16
                                 *      .psect  $pdata  ,ro,d,lcl,rel,con
                                 *               ; k11mac.mac, first pass only
                                 *      .psect  $PDATA  ,D      ; line 1083
                                 * and ends up with
                                 * $PDATA  001074    003   (RO,D,LCL,REL,CON)
                                 */

                                /*
                                sect->flags = old_flags;
                                report(stack->top, "Program section flags not identical\n");
                                */
                            }
                        }

                        if (!enabl_lsb) {
                            lsb = get_next_lsb();
                        }
                        go_section(tr, sect);
                        list_location(stack->top, DOT);

                        return CHECK_EOL;
                    }                  /* end PSECT code */
                    break;

                case P_WEAK:
                case P_GLOBL:
                    {
                        SYMBOL         *sym;
                        int             islocal = 0;

                        while (!EOL(*cp)) {
                            /* Loop and make definitions for
                               comma-separated symbols */
                            label = get_symbol(cp, &ncp, &islocal);
                            if (label == NULL) {
                                report(stack->top, "Bad .GLOBL/.WEAK syntax (symbol expected)\n");
                                return 0;
                            }

                            if (islocal) {
                                report(stack->top, "Local label used in .GLOBL/.WEAK\n");
                                return 0;
                            }

                            sym = lookup_sym(label, &symbol_st);
                            if (sym) {
                                sym->flags |= SYMBOLFLAG_GLOBAL | (op->value == P_WEAK ? SYMBOLFLAG_WEAK : 0);
                            } else
                                sym = add_sym(label, 0,
                                        SYMBOLFLAG_GLOBAL |
                                            (op->value == P_WEAK ? SYMBOLFLAG_WEAK : 0),
                                        &absolute_section, &symbol_st, stack->top);

                            free(label);
                            cp = skipdelim(ncp);
                        }
                    }
                    return CHECK_EOL;

                case P_WORD:
                    {
                        /* .WORD might be followed by nothing, which
                           is an implicit .WORD 0 */
                        if (EOL(*cp)) {
                            if (DOT & 1) {
                                report(stack->top, ".WORD on odd boundary\n");
                                DOT++; /* Fix it, too */
                            }
                            store_word(stack->top, tr, 2, 0);
                            return 1;
                        } else
                            return do_word(stack, tr, cp, 2);
                    }

                case P_BYTE:
                    if (EOL(*cp)) {
                        /* Blank .BYTE.  Same as .BYTE 0 */
                        store_word(stack->top, tr, 1, 0);
                        return 1;
                    } else
                        return do_word(stack, tr, cp, 1);

                case P_BLKW:
                case P_BLKB:
                    {
                        EX_TREE        *value;
                        int             ok = 1;

                        cp = skipwhite(cp);
                        if (EOL(*cp)) {
                            /* If no argument, assume 1. Documented but
                             * discouraged. Par 6.5.3, page 6-32. */
                            /* warning(stack->top, "Argument to .BLKB/.BLKW should be present; 1 assumed\n"); */
                            value = new_ex_lit(1);
                        } else {
                            value = parse_expr(cp, 0);
                            cp = value->cp;
                        }

                        if (value->type != EX_LIT) {
                            report(stack->top, "Argument to .BLKB/.BLKW must be constant\n");
                            ok = 0;
                        } else {
                            list_value(stack->top, DOT);
                            DOT += value->data.lit * (op->value == P_BLKW ? 2 : 1);
                            change_dot(tr, 0);
                        }
                        free_tree(value);
                        return ok && CHECK_EOL;
                    }

                case P_ASCIZ:
                case P_ASCII:
                    {
                        do {
                            cp = skipwhite(cp);
                            if (*cp == '<') {
                                EX_TREE        *value;
                                /* A byte value */
                                value = parse_unary_expr(cp, 0);
                                cp = value->cp;
                                store_value(stack, tr, 1, value);
                                free_tree(value);
                            } else {
                                char            quote = *cp++;

                                while (*cp && *cp != '\n' && *cp != quote)
                                    store_word(stack->top, tr, 1, *cp++);
                                cp++;  /* Skip closing quote */
                            }

                            cp = skipwhite(cp);
                        } while (!EOL(*cp));

                        if (op->value == P_ASCIZ) {
                            store_word(stack->top, tr, 1, 0);
                        }

                        return 1;
                    }

                case P_RAD50:
                    if (DOT & 1) {
                        report(stack->top, ".RAD50 on odd boundary\n");
                        DOT++;         /* Fix it */
                    }
                    {
                        int i, count;
                        unsigned *rad50;

                        /* Now assemble the argument */
                        rad50 = assemble_rad50 (cp, 0, &count, stack);
                        for (i = 0; i < count; i++) {
                            store_word (stack->top, tr, 2, rad50[i]);
                        }
                        free (rad50);
                    }
                    return 1;

                case P_PACKED:
#define PACKED_POSITIVE 0x0C
#define PACKED_NEGATIVE 0x0D
#define PACKED_UNSIGNED 0x0F
                    {
                        int sign;       /* The sign comes at the end */

                        cp = skipwhite(cp);

                        if (*cp == '+') {
                            sign = PACKED_POSITIVE;
                            cp++;
                        } else if (*cp == '-') {
                            sign = PACKED_NEGATIVE;
                            cp++;
                        } else {
                            sign = PACKED_UNSIGNED;
                        }

                        /* Count number of digits */
                        int ndigits = 0;
                        for (char *tmp = cp;
                                isdigit((unsigned char )*tmp);
                                tmp++) {
                            ndigits++;
                        }

                        if (ndigits > 31) {
                            report(stack->top, "Too many packed decimal digits\n");
                            return 1;
                        }

                        /* If the number of digits is even,
                         * prefix an imaginary zero. */
                        int nybbles = !(ndigits % 2);
                        int byte = 0;

                        while (isdigit((unsigned char)*cp)) {
                            int value = *cp - '0';
                            byte = (byte << 4) + value;
                            nybbles++;
                            if ((nybbles % 2) == 0) {
                                store_word(stack->top, tr, 1, byte);
                                byte = 0;
                            }
                            cp++;
                        }

                        /* Append the sign, making an even number of nybbles. */
                        byte = (byte << 4) + sign;
                        store_word(stack->top, tr, 1, byte);

                        /* Maybe store the number of digits into a symbol. */
                        cp = skipdelim(cp);
                        if (!EOL(*cp)) {
                            int islocal;

                            label = get_symbol(cp, &cp, &islocal);

                            if (label == NULL) {
                                report(stack->top, "Bad .PACKED syntax (symbol expected)\n");
                                return 0;
                            }

                            add_sym(label, ndigits, SYMBOLFLAG_DEFINITION | islocal,
                                    &absolute_section, &symbol_st, stack->top);
                            free(label);
                        }

                    }
                    return CHECK_EOL;

                default:
                    report(stack->top, "Unimplemented directive %s\n", op->label);
                    return 0;
                }                      /* end switch (PSEUDO operation) */

            case SECTION_INSTRUCTION:
                {
                    /* The PC must always be even. */
                    if (DOT & 1) {
                        report(stack->top, "Instruction on odd address\n");
                        DOT++;         /* ...and fix it... */
                    }

                    switch (op->flags & OC_MASK) {
                    case OC_NONE:
                        /* No operands. */
                        store_word(stack->top, tr, 2, op->value);
                        return CHECK_EOL;

                    case OC_MARK:
                        /* MARK, EMT, TRAP */  {
                            EX_TREE        *value;

                            cp = skipwhite(cp);
                            if (EOL (*cp)) {
                                /* Default argument is 0 */
                                store_word (stack->top, tr, 2, op->value);
                            }
                            else {
                                if (*cp == '#')
                                    cp++;  /* Allow the hash, but
                                              don't require it */
                                value = parse_expr(cp, 0);
                                cp = value->cp;
                                if (value->type != EX_LIT) {
                                    if (op->value == I_MARK) {
                                        report(stack->top,
                                               "Instruction requires simple literal operand\n");
                                        store_word(stack->top, tr, 2, op->value);
                                    }
                                    else {
                                        /* EMT, TRAP: handle as two bytes */
                                        store_value (stack, tr, 1, value);
                                        store_word (stack->top, tr, 1, op->value >> 8);
                                    }
                                } else {
                                    unsigned v = value->data.lit;
                                    unsigned int max = (op->value == I_MARK)? 077 : 0377;

                                    if (v > max) {
                                        report(stack->top, "Literal operand too large (%d. > %d.)\n", value->data.lit, max);
                                        v = max;
                                    }
                                    store_word(stack->top, tr, 2, op->value | v);
                                }
                                free_tree(value);
                            }
                        }
                        return CHECK_EOL;

                    case OC_1GEN:
                        /* One general addressing mode */  {
                            ADDR_MODE       mode;
                            unsigned        word;
                            char           *error;

                            if (!get_mode(cp, &cp, &mode, &error)) {
                                report(stack->top,
                                       "Invalid addressing mode (%s)\n", error);
                                return 0;
                            }

                            if (op->value == I_JMP && (mode.type & 070) == 0) {
                                report(stack->top, "JMP Rn is impossible\n");
                                /* But encode it anyway... */
                            }

                            /* Build instruction word */
                            word = op->value | mode.type;
                            store_word(stack->top, tr, 2, word);
                            mode_extension(tr, &mode, stack->top);
                        }
                        return CHECK_EOL;

                    case OC_2GEN:
                        /* Two general addressing modes */  {
                            ADDR_MODE       left,
                                            right;
                            unsigned        word;
                            char           *error;

                            if (!get_mode(cp, &cp, &left, &error)) {
                                report(stack->top,
                                       "Invalid addressing mode (1st operand: %s)\n",
                                       error);
                                return 0;
                            }

                            cp = skipwhite(cp);
                            if (*cp++ != ',') {
                                report(stack->top, "Invalid syntax (comma expected)\n");
                                free_addr_mode(&left);
                                return 0;
                            }

                            if (!get_mode(cp, &cp, &right, &error)) {
                                report(stack->top,
                                       "Invalid addressing mode (2nd operand: %s)\n",
                                       error);
                                free_addr_mode(&left);
                                return 0;
                            }

                            /* Build instruction word */
                            word = op->value | left.type << 6 | right.type;
                            store_word(stack->top, tr, 2, word);
                            mode_extension(tr, &left, stack->top);
                            mode_extension(tr, &right, stack->top);
                        }
                        return CHECK_EOL;

                    case OC_BR:
                        /* branches */  {
                            EX_TREE        *value;
                            unsigned        offset;

                            value = parse_expr(cp, 0);
                            cp = value->cp;

                            /* Relative PSECT or absolute? */
                            if (current_pc->section->flags & PSECT_REL) {
                                SYMBOL         *sym = NULL;

                                /* Can't branch unless I can
                                   calculate the offset. */

                                /* You know, I *could* branch
                                   between sections if I feed the
                                   linker a complex relocation
                                   expression to calculate the
                                   offset.  But I won't. */

                                if (!express_sym_offset(value, &sym, &offset)
                                    || sym->section != current_pc->section) {
                                    report(stack->top, "Bad branch target (%s)\n",
                                            sym ? "not same section"
                                                : "can't express offset");
                                    store_word(stack->top, tr, 2, op->value);
                                    free_tree(value);
                                    return 0;
                                }

                                /* Compute the branch offset and
                                   check for addressability */
                                offset += sym->value;
                                offset -= DOT + 2;
                            } else {
                                if (value->type != EX_LIT) {
                                    report(stack->top, "Bad branch target (not literal; ABS section)\n");
                                    store_word(stack->top, tr, 2, op->value);
                                    free_tree(value);
                                    return 0;
                                }

                                offset = value->data.lit - (DOT + 2);
                            }

                            if (!check_branch(stack, offset, -256, 255))
                                offset = 0;

                            /* Emit the branch code */
                            offset &= 0777;     /* Reduce to 9 bits */
                            offset >>= 1;       /* Shift to become
                                                   word offset */

                            store_word(stack->top, tr, 2, op->value | offset);

                            free_tree(value);
                        }
                        return CHECK_EOL;

                    case OC_SOB:
                        {
                            EX_TREE        *value;
                            unsigned        reg;
                            unsigned        offset;

                            value = parse_expr(cp, 0);
                            cp = value->cp;

                            reg = get_register(value);
                            free_tree(value);
                            if (reg == NO_REG) {
                                report(stack->top, "Invalid addressing mode (register expected)\n");
                                return 0;
                            }

                            cp = skipwhite(cp);
                            if (*cp++ != ',') {
                                report(stack->top, "Invalid syntax (comma expected)\n");
                                return 0;
                            }

                            value = parse_expr(cp, 0);
                            cp = value->cp;

                            /* Relative PSECT or absolute? */
                            if (current_pc->section->flags & PSECT_REL) {
                                SYMBOL         *sym;

                                if (!express_sym_offset(value, &sym, &offset)) {
                                    report(stack->top, "Bad branch target (can't express offset)\n");
                                    free_tree(value);
                                    return 0;
                                }
                                /* Must be same section */
                                if (sym->section != current_pc->section) {
                                    report(stack->top, "Bad branch target (different section)\n");
                                    free_tree(value);
                                    offset = 0;
                                } else {
                                    /* Calculate byte offset */
                                    offset += DOT + 2;
                                    offset -= sym->value;
                                }
                            } else {
                                if (value->type != EX_LIT) {
                                    report(stack->top, "Bad branch target (not a literal)\n");
                                    offset = 0;
                                } else {
                                    offset = DOT + 2 - value->data.lit;
                                }
                            }

                            if (!check_branch(stack, offset, 0, 126))
                                offset = 0;

                            offset &= 0177;     /* Reduce to 7 bits */
                            offset >>= 1;       /* Shift to become word offset */
                            store_word(stack->top, tr, 2, op->value | offset | (reg << 6));

                            free_tree(value);
                        }
                        return CHECK_EOL;

                    case OC_ASH:
                        /* First op is gen, second is register. */  {
                            ADDR_MODE       mode;
                            EX_TREE        *value;
                            unsigned        reg;
                            unsigned        word;
                            char           *error;

                            if (!get_mode(cp, &cp, &mode, &error)) {
                                report(stack->top,
                                       "Invalid addressing mode (1st operand: %s)\n",
                                       error);
                                return 0;
                            }

                            cp = skipwhite(cp);
                            if (*cp++ != ',') {
                                report(stack->top, "Invalid syntax (comma expected)\n");
                                free_addr_mode(&mode);
                                return 0;
                            }
                            value = parse_expr(cp, 0);
                            cp = value->cp;

                            reg = get_register(value);
                            if (reg == NO_REG) {
                                report(stack->top, "Invalid addressing mode (2nd operand: register expected)\n");
                                free_tree(value);
                                free_addr_mode(&mode);
                                return 0;
                            }

                            /* Instruction word */
                            word = op->value | mode.type | (reg << 6);
                            store_word(stack->top, tr, 2, word);
                            mode_extension(tr, &mode, stack->top);
                            free_tree(value);
                        }
                        return CHECK_EOL;

                    case OC_JSR:
                        /* For JSR and XOR, first op is register, second is gen. */  {
                            ADDR_MODE       mode;
                            EX_TREE        *value;
                            unsigned        reg;
                            unsigned        word;
                            char           *error;

                            value = parse_expr(cp, 0);
                            cp = value->cp;

                            reg = get_register(value);
                            if (reg == NO_REG) {
                                report(stack->top, "Invalid addressing mode (1st operand: register exected)\n");
                                free_tree(value);
                                return 0;
                            }

                            cp = skipwhite(cp);
                            if (*cp++ != ',') {
                                report(stack->top, "Invalid syntax (comma expected)\n");
                                return 0;
                            }

                            if (!get_mode(cp, &cp, &mode, &error)) {
                                report(stack->top,
                                       "Invalid addressing mode (2nd operand: %s)\n",
                                       error);
                                free_tree(value);
                                return 0;
                            }

                            if (op->value == I_JSR && (mode.type & 070) == 0) {
                                report(stack->top, "JSR Rn,Rm is impossible\n");
                                /* But encode it anyway... */
                            }

                            word = op->value | mode.type | (reg << 6);
                            store_word(stack->top, tr, 2, word);
                            mode_extension(tr, &mode, stack->top);
                            free_tree(value);
                        }
                        return CHECK_EOL;

                    case OC_1REG:
                        /* One register (RTS,FADD,FSUB,FMUL,FDIV,SPL) */  {
                            EX_TREE        *value;
                            unsigned        reg;

                            value = parse_expr(cp, 0);
                            cp = value->cp;
                            reg = get_register(value);
                            if (reg == NO_REG) {
                                report(stack->top, "Invalid addressing mode (register expected)\n");
                                reg = 0;
                            }

                            store_word(stack->top, tr, 2, op->value | reg);
                            free_tree(value);
                        }
                        return CHECK_EOL;

#if 0
/*
 * Although it is arguable that the FPP TSTF/TSTD instruction has 1
 * operand which is a floating point source, the PDP11 Architecture
 * Handbook describes it as a destination, and MACRO11 V05.05 doesn't
 * allow a FP literal argument.
 */
                    case OC_FPP_FSRC:
                        /* One fp immediate or a general addressing mode */  {
                            ADDR_MODE       mode;
                            unsigned        word;

                            if (!get_fp_src_mode(cp, &cp, &mode)) {
                                report(stack->top, "Invalid addressing mode\n");
                                return 0;
                            }

                            /* Build instruction word */
                            word = op->value | mode.type;
                            store_word(stack->top, tr, 2, word);
                            mode_extension(tr, &mode, stack->top);
                        }
                        return CHECK_EOL;
#endif

                    case OC_FPP_SRCAC:
                    case OC_FPP_FSRCAC:
                        /* One gen and one reg 0-3 */  {
                            ADDR_MODE       mode;
                            EX_TREE        *value;
                            unsigned        reg;
                            unsigned        word;
                            char           *error;

                            if ((op->flags & OC_MASK) == OC_FPP_FSRCAC) {
                                if (!get_fp_src_mode(cp, &cp, &mode, &error)) {
                                    report(stack->top,
                                           "Invalid addressing mode (1st operand, fsrc: %s)\n",
                                           error);
                                    return 0;
                                }
                            } else {
                                if (!get_mode(cp, &cp, &mode, &error)) {
                                    report(stack->top,
                                           "Invalid addressing mode (1st operand: %s)\n",
                                           error);
                                    return 0;
                                }
                            }

                            cp = skipwhite(cp);
                            if (*cp++ != ',') {
                                report(stack->top, "Invalid syntax (comma expected)\n");
                                free_addr_mode(&mode);
                                return 0;
                            }

                            value = parse_expr(cp, 0);
                            cp = value->cp;

                            reg = get_register(value);
                            if (reg == NO_REG || reg > 3) {
                                report(stack->top, "Invalid destination fp register\n");
                                reg = 0;
                            }

                            /*
                             * We could check here that the general mode
                             * is not AC6 or AC7, but the original Macro11
                             * doesn't do that either.
                             */
                            word = op->value | mode.type | (reg << 6);
                            store_word(stack->top, tr, 2, word);
                            mode_extension(tr, &mode, stack->top);
                            free_tree(value);
                        }
                        return CHECK_EOL;

                    case OC_FPP_ACFDST:
                        /* One reg 0-3 and one fdst */  {
                            ADDR_MODE       mode;
                            EX_TREE        *value;
                            unsigned        reg;
                            unsigned        word;
                            char           *error;

                            value = parse_expr(cp, 0);
                            cp = value->cp;

                            reg = get_register(value);
                            if (reg == NO_REG || reg > 3) {
                                report(stack->top, "Invalid source fp register\n");
                                reg = 0;
                            }

                            cp = skipwhite(cp);
                            if (*cp++ != ',') {
                                report(stack->top, "Invalid syntax (comma expected)\n");
                                free_tree(value);
                                return 0;
                            }

                            if (!get_mode(cp, &cp, &mode, &error)) {
                                report(stack->top,
                                       "Invalid addressing mode (2nd operand: %s)\n",
                                       error);
                                free_tree(value);
                                return 0;
                            }

                            /*
                             * We could check here that the general mode
                             * is not AC6 or AC7, but the original Macro11
                             * doesn't do that either.
                             *
                             * For some (mostly STore instructions) the
                             * destination isn't a FDST but a plain DST.
                             */
                            word = op->value | mode.type | (reg << 6);
                            store_word(stack->top, tr, 2, word);
                            mode_extension(tr, &mode, stack->top);
                            free_tree(value);
                        }
                        return CHECK_EOL;

                        {   int nwords;
                            EX_TREE *expr[4];
                    case OC_CIS2:
                        /* Either no operands or 2 (mostly) address operand words
                         * (extension) */
                            nwords = 2;
                            goto cis_common;
                    case OC_CIS3:
                        /* Either no operands or 3 (mostly) address operand words
                         * (extension) */
                            nwords = 3;
                            goto cis_common;
                    case OC_CIS4:
                        /* Either no operands or 4 (mostly) address operand words
                         * (extension) */
                            nwords = 4;
                    cis_common:
                            if (!EOL(*cp)) {
                                for (int i = 0; i < nwords; i++) {
                                    if (i > 0) {
                                        cp = skipwhite(cp);
                                        if (*cp++ != ',') {
                                            report(stack->top, "Invalid syntax (operand %d: comma expected)\n", i+1);
                                            cp--;
                                        }
                                    }
                                    EX_TREE *ex = parse_expr(cp, 0);
                                    if (!expr_ok(ex)) {
                                        report(stack->top, "Invalid expression (operand %d)\n", i+1);
                                    }
                                    cp = ex->cp;
                                    expr[i] = ex;
                                }
                            } else {
                                expr[0] = NULL;
                            }

                            store_word(stack->top, tr, 2, op->value);

                            if (expr[0]) {
                                for (int i = 0; i < nwords; i++) {
                                    store_value(stack, tr, 2, expr[i]);
                                }
                            }
                        }
                        return CHECK_EOL;

                    default:
                        report(stack->top, "Unimplemented instruction format\n");
                        return 0;
                    }                  /* end(handle an instruction) */
                }
                break;
            }                          /* end switch(section type) */
        }                              /* end if (op is a symbol) */
    }

    /* If .ENABL MCL is in effect, try to define the symbol as a
     * library macro if it is not a defined symbol. */
    if (enabl_mcl) {
        if (lookup_sym(label, &symbol_st) == NULL) {
            if (do_mcall (label, stack)) {
                goto do_mcalled_macro;
            }
        }
    }

    /* Only thing left is an implied .WORD directive */
    /*JH: fall through in case of illegal opcode, illegal label! */
    free(label);

    return do_word(stack, tr, cp, 2);
}

int get_next_lsb(
    void)
{
    if (lsb_used) {
        lsb_used = 0;
        if (enabl_debug && lstfile) {
            fprintf(lstfile, "get_next_lsb: lsb: %d becomes %d (= next_lsb)\n", lsb, next_lsb);
        }
        return next_lsb++;
    } else {
        if (enabl_debug && lstfile) {
            fprintf(lstfile, "get_next_lsb: lsb: stays %d\n", lsb);
        }
        return lsb;
    }
}

/* assemble_stack assembles the input stack.  It returns the error
   count. */

int assemble_stack(
    STACK *stack,
    TEXT_RLD *tr)
{
    int             res;
    int             errcount = 0;

    while ((res = assemble(stack, tr)) >= 0) {
        list_flush();
        if (res == 0)
            errcount++;                   /* Count an error */
    }

    return errcount;
}
