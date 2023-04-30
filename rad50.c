/* Functions to convert RAD50 to or from ASCII. */

/*
Copyright (c) 2001, Richard Krehbiel
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

o Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

o Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

o Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.

*/

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "rad50.h"  /* My own definitions */


static const char   rad50charset[]    = " ABCDEFGHIJKLMNOPQRSTUVWXYZ$. 0123456789";
static const char   rad50charset_ul[] = " ABCDEFGHIJKLMNOPQRSTUVWXYZ$._0123456789";
static const char  *rad50tbl          = rad50charset;


/* rad50_enable_underscore - enables '_' as a valid character in a RAD50 string */

void rad50_enable_underscore(
    void)
{
    rad50tbl = rad50charset_ul;
}


/* rad50 - converts from 0 to 3 ASCII (or EBCDIC, if your compiler is so
   inclined) characters into a RAD50 word. */

unsigned rad50(
    char *cp,
    char **endp)
{
    unsigned long   acc = 0;
    char           *rp;

    if (endp)
        *endp = cp;
    if (!*cp)                      /* Got to check for end-of-string manually, because strchr will call it a hit. */
        return acc;

    rp = strchr(rad50tbl, toupper((unsigned char)*cp));
    if (rp == NULL)                /* Not a RAD50 character */
        return acc;
    acc = ((int) (rp - rad50tbl)) * 03100;        /* Convert */
    cp++;

    /* Now, do the same thing two more times... */

    if (endp)
        *endp = cp;
    if (!*cp)
        return acc;

    rp = strchr(rad50tbl, toupper((unsigned char)*cp));
    if (rp == NULL)
        return acc;
    acc += ((int) (rp - rad50tbl)) * 050;
    cp++;

    if (endp)
        *endp = cp;
    if (!*cp)
        return acc;

    rp = strchr(rad50tbl, toupper((unsigned char)*cp));
    if (rp == NULL)
        return acc;

    acc += (int) (rp - rad50tbl);
    cp++;

    if (endp)
        *endp = cp;
    return acc;                        /* Done. */
}


/* rad50x2 - converts from 0 to 6 characters into two words of RAD50. */

void rad50x2(
    char *cp,
    unsigned *rp)
{
    *rp++ = rad50(cp, &cp);
    *rp = 0;
    if (*cp)
        *rp = rad50(cp, &cp);
}


/* unrad50 - converts a RAD50 word to three characters of ASCII. */

void unrad50(
    unsigned word,
    char *cp)
{
    if (word < 0175000) {              /* Is it legal RAD50? */
        cp[0] = rad50charset_ul[word / 03100];
        cp[1] = rad50charset_ul[(word / 050) % 050];
        cp[2] = rad50charset_ul[word % 050];
    } else
        cp[0] = cp[1] = cp[2] = '?';
}


/* ascii2rad50 - convert a single character to a RAD50 character */

int ascii2rad50(
    char c)
{
    char           *rp;

    if (c == '\0')                     /* Not a RAD50 character */
        return -1;
    rp = strchr(rad50tbl, toupper((unsigned char)c));
    if (rp == NULL)                    /* Not a RAD50 character */
        return -1;
    return (int) (rp - rad50tbl);        /* Convert */
}


/* packrad50word - packs up to 3 characters into a RAD50 word.
 *
 * The characters should be in the range [0:047]),
 * such as having been converted by ascii2rad50().
 */

unsigned packrad50word(
    char *cp,
    int len)
{
    unsigned long   acc = 0;

    if (len >= 1) {
        acc += (cp[0] % 050) * 050 * 050;
    }
    if (len >= 2) {
        acc += (cp[1] % 050) * 050;
    }
    if (len >= 3) {
        acc += cp[2] % 050;
    }

    return acc;
}
