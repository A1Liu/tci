/*	$OpenBSD: vfscanf.c,v 1.21 2006/01/13 21:33:28 millert Exp $ */
/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <ctype.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <tci.h>

#define BUF 513 /* Maximum length of numeric string. */

#define LONG 0x00001       /* l: long or double */
#define LONGDBL 0x00002    /* L: long double; unimplemented */
#define SHORT 0x00004      /* h: short */
#define SHORTSHORT 0x00008 /* hh: 8 bit integer */
#define LLONG 0x00010      /* ll: long long (+ deprecated q: quad) */
#define POINTER 0x00020    /* p: void * (as hex) */
#define SIZEINT 0x00040    /* z: (signed) size_t */
#define MAXINT 0x00080     /* j: intmax_t */
#define PTRINT 0x00100     /* t: ptrdiff_t */
#define NOSKIP 0x00200     /* [ or c: do not skip blanks */
#define SUPPRESS 0x00400   /* *: suppress assignment */
#define UNSIGNED 0x00800   /* %[oupxX] conversions */

/*
 * The following are used in numeric conversions only:
 * SIGNOK, HAVESIGN, NDIGITS, DPTOK, and EXPOK are for floating point;
 * SIGNOK, HAVESIGN, NDIGITS, PFXOK, and NZDIGITS are for integral.
 */
#define SIGNOK 0x01000   /* +/- is (still) legal */
#define HAVESIGN 0x02000 /* sign detected */
#define NDIGITS 0x04000  /* no digits detected */
#define DPTOK 0x08000    /* (float) decimal point is still legal */
#define EXPOK 0x10000    /* (float) exponent (e+3, etc) still legal */
#define PFXOK 0x08000    /* 0x prefix is (still) legal */
#define NZDIGITS 0x10000 /* no zero digits detected */

/*
 * Conversion types.
 */
#define CT_CHAR 0   /* %c conversion */
#define CT_CCL 1    /* %[...] conversion */
#define CT_STRING 2 /* %s conversion */
#define CT_INT 3    /* integer, i.e., strtoimax or strtoumax */
#define CT_FLOAT 4  /* floating, i.e., strtod */
#define u_char unsigned char
#define u_long unsigned long

static u_char *__sccl(char *tab, u_char *fmt);

int scanf(const char *fmt, ...) {
  va_list list;
  va_start(list, fmt);

  int ret = vfscanf(stdin, fmt, list);
  va_end(list);

  return ret;
}

int fscanf(FILE *fp, const char *fmt, ...) {
  va_list list;
  va_start(list, fmt);

  int ret = vfscanf(fp, fmt, list);
  va_end(list);

  return ret;
}

int vfscanf(FILE *fp, const char *fmt, va_list ap) {
  int nassigned = 0, nread = 0;
  char ccltab[256], num_buf[BUF];
  static short basefix[17] = {10, 1,  2,  3,  4,  5,  6,  7, 8,
                              9,  10, 11, 12, 13, 14, 15, 16};

  for (;;) {
    char c = *fmt++;
    int flags = 0, base = 0;
    size_t width = 0;

    if (c == 0)
      return nassigned;

    if (isspace(c)) {
      char in;
      while (isspace(in = fgetc(fp)))
        nread++;

      ungetc(in, fp);
      continue;
    }

    if (c != '%') { // read in a literal
      char in = fgetc(fp);
      if (in == EOF)
        return (ungetc(in, fp), nassigned ? nassigned : -1);
      if (in != c)
        return (ungetc(in, fp), nassigned);

      nread++;
      continue;
    }

  again:
    c = *fmt++;
    switch (c) {
    case '%': {
      char in = fgetc(fp);
      if (in == EOF)
        return (ungetc(in, fp), nassigned ? nassigned : -1);
      if (in != c)
        return (ungetc(in, fp), nassigned);

      nread++;
      continue;
    }

    case '*':
      flags |= SUPPRESS;
      goto again;
    case 'j':
      flags |= MAXINT;
      goto again;
    case 'L':
      flags |= LONGDBL;
      goto again;
    case 'h':
      if (*fmt == 'h') {
        fmt++;
        flags |= SHORTSHORT;
      } else {
        flags |= SHORT;
      }
      goto again;
    case 'l':
      if (*fmt == 'l') {
        fmt++;
        flags |= LLONG;
      } else {
        flags |= LONG;
      }
      goto again;
    case 'q':
      flags |= LLONG; /* deprecated */
      goto again;
    case 't':
      flags |= PTRINT;
      goto again;
    case 'z':
      flags |= SIZEINT;
      goto again;

    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      width = width * 10 + c - '0';
      goto again;

    /*
     * Conversions.
     * Those marked `compat' are for 4.[123]BSD compatibility.
     *
     * (According to ANSI, E and X formats are supposed
     * to the same as e and x.  Sorry about that.)
     */
    case 'D': /* compat */
      flags |= LONG;
      /* FALLTHROUGH */
    case 'd':
      c = CT_INT;
      base = 10;
      break;

    case 'i':
      c = CT_INT;
      base = 0;
      break;

    case 'O': /* compat */
      flags |= LONG;
      /* FALLTHROUGH */
    case 'o':
      c = CT_INT;
      flags |= UNSIGNED;
      base = 8;
      break;

    case 'u':
      c = CT_INT;
      flags |= UNSIGNED;
      base = 10;
      break;

    case 'X':
    case 'x':
      flags |= PFXOK; /* enable 0x prefixing */
      c = CT_INT;
      flags |= UNSIGNED;
      base = 16;
      break;

    case 'E':
    case 'G':
    case 'e':
    case 'f':
    case 'g':
      c = CT_FLOAT;
      break;

    case 's':
      c = CT_STRING;
      break;

    case '[':
      fmt = __sccl(ccltab, fmt);
      flags |= NOSKIP;
      c = CT_CCL;
      break;

    case 'c':
      flags |= NOSKIP;
      c = CT_CHAR;
      break;

    case 'p': // TODO THIS IS WILDLY UNSAFE WHO THOUGHT THIS WAS OK
      flags |= POINTER | PFXOK;
      c = CT_INT;
      flags |= UNSIGNED;
      base = 16;
      break;

    case 'n':
      if (flags & SUPPRESS)
        continue;
      if (flags & SHORTSHORT)
        *va_arg(ap, char *) = nread;
      else if (flags & SHORT)
        *va_arg(ap, short *) = nread;
      else if (flags & LONG)
        *va_arg(ap, long *) = nread;
      else if (flags & SIZEINT)
        *va_arg(ap, ssize_t *) = nread;
      else if (flags & PTRINT)
        *va_arg(ap, ptrdiff_t *) = nread;
      else if (flags & LLONG)
        *va_arg(ap, long long *) = nread;
      else if (flags & MAXINT)
        *va_arg(ap, intmax_t *) = nread;
      else
        *va_arg(ap, int *) = nread;
      continue;

    case '\0': // TODO should this be how it behaves?
      return EOF;
    default:
      if (isupper(c))
        flags |= LONG;
      c = CT_INT;
      base = 10;
      break;
    }

    {
      char in = fgetc(fp);
      if (in == EOF) // if there's no more input left, return
        return nassigned ? nassigned : -1;
      ungetc(in, fp);
    }

    if (!(flags & NOSKIP)) {
      char in;
      while (isspace(in = fgetc(fp)))
        nread++;

      if (in == EOF)
        return nassigned;
      ungetc(in, fp);
    }

    // Do the conversion
    switch (c) {
    case CT_CHAR: {
      /* scan arbitrary characters (sets NOSKIP) */
      width = width == 0 ? 1 : width;
      char in, *out = (flags & SUPPRESS) ? NULL : va_arg(ap, char *);
      size_t i = 0;

      for (; i < width && (in = fgetc(fp)) != EOF; i++)
        if (out)
          out[i] = in;

      if (i == 0)
        return nassigned;

      nread += i;
      if (out)
        nassigned++;
    } break;

    case CT_CCL: {
      /* scan a (nonempty) character class (sets NOSKIP) */
      width = (width == 0) ? (size_t)~0 : width;
      char in = 0, *p = (flags & SUPPRESS) ? NULL : va_arg(ap, char *);
      size_t i = 0;

      for (; i < width && (in = fgetc(fp)) != EOF && ccltab[in]; i++)
        if (p)
          p[i] = in;

      if (i == 0)
        return nassigned;

      if (p)
        p[i] = '\0', nassigned++;

      nread += i;
    } break;

    case CT_STRING: {
      /* like CCL, but zero-length string OK, & no NOSKIP */
      width = (width == 0) ? (size_t)~0 : width;
      char in = 0, *p = (flags & SUPPRESS) ? NULL : va_arg(ap, char *);
      size_t i = 0;

      for (; i < width && (in = fgetc(fp)) != EOF && !isspace(in); i++)
        if (p)
          p[i] = in;

      if (p)
        p[i] = '\0', nassigned++;

      nread += i;
    } break;

    case CT_INT: {
      /* scan an integer as if by strtoimax/strtoumax */

      width = (width == 0 || width > BUF - 1) ? BUF - 1 : width;
      flags |= SIGNOK | NDIGITS | NZDIGITS;
      char in, *p = num_buf;

      for (; width && (in = fgetc(fp)) != EOF; width--) {

        /* Switch on the character; `goto ok' if we accept it as a part of
         * number. */
        switch (in) {
        case '0':
          if (base == 0) {
            base = 8;
            flags |= PFXOK;
          }
          if (flags & NZDIGITS)
            flags &= ~(SIGNOK | NZDIGITS | NDIGITS);
          else
            flags &= ~(SIGNOK | PFXOK | NDIGITS);
          goto ok;

        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
          base = basefix[base];
          flags &= ~(SIGNOK | PFXOK | NDIGITS);
          goto ok;

        case '8':
        case '9':
          base = basefix[base];
          if (base <= 8)
            break; /* not legal here */
          flags &= ~(SIGNOK | PFXOK | NDIGITS);
          goto ok;

        case 'A':
        case 'B':
        case 'C':
        case 'D':
        case 'E':
        case 'F':
        case 'a':
        case 'b':
        case 'c':
        case 'd':
        case 'e':
        case 'f':
          /* no need to fix base here */
          if (base <= 10)
            break; /* not legal here */
          flags &= ~(SIGNOK | PFXOK | NDIGITS);
          goto ok;

        case '+':
        case '-':
          if (flags & SIGNOK) {
            flags &= ~SIGNOK;
            flags |= HAVESIGN;
            goto ok;
          }
          break;

        case 'x':
        case 'X':
          if ((flags & PFXOK) && p == num_buf + 1 + !!(flags & HAVESIGN)) {
            base = 16; /* if %i */
            flags &= ~PFXOK;
            goto ok;
          }
          break;
        }

        // 'in' is not a legal character
        ungetc(in, fp);
        break;
      ok:
        *p++ = in;
      }

      // If we had only a sign, it is no good; push back the sign.
      if (flags & NDIGITS) {
        if (p > num_buf)
          ungetc(*--p, fp);
        return nassigned;
      }

      /* If the number ends in `x', it was [sign] '0' 'x', so push back the x
       * and treat it as [sign] '0'.  */
      if (p[-1] == 'x' || p[-1] == 'X')
        ungetc(*--p, fp);

      if (!(flags & SUPPRESS)) {
        *p = '\0';
        uintmax_t res = (flags & UNSIGNED) ? strtoumax(num_buf, NULL, base)
                                           : strtoimax(num_buf, NULL, base);

        if (flags & POINTER)
          *va_arg(ap, void **) = (void *)(uintptr_t)res;
        else if (flags & MAXINT)
          *va_arg(ap, intmax_t *) = res;
        else if (flags & LLONG)
          *va_arg(ap, long long *) = res;
        else if (flags & SIZEINT)
          *va_arg(ap, ssize_t *) = res;
        else if (flags & PTRINT)
          *va_arg(ap, ptrdiff_t *) = res;
        else if (flags & LONG)
          *va_arg(ap, long *) = res;
        else if (flags & SHORT)
          *va_arg(ap, short *) = res;
        else if (flags & SHORTSHORT)
          *va_arg(ap, char *) = res;
        else
          *va_arg(ap, int *) = res;
        nassigned++;
      }

      nread += p - num_buf;
    } break;

    case CT_FLOAT: {
      tci_throw_error("NoFloatingPointSupport",
                      "TCI doesn't support floating point right now", 0);

      width = (width == 0 || width > BUF - 1) ? BUF - 1 : width;
      char in, *p = num_buf;

      flags |= SIGNOK | NDIGITS | DPTOK | EXPOK;
      for (; width && (in = fgetc(fp)) != EOF; width--) {
        switch (in) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
          flags &= ~(SIGNOK | NDIGITS);
          goto fok;
        case '+':
        case '-':
          if (flags & SIGNOK) {
            flags &= ~SIGNOK;
            goto fok;
          }
          break;
        case '.':
          if (flags & DPTOK) {
            flags &= ~(SIGNOK | DPTOK);
            goto fok;
          }
          break;
        case 'e':
        case 'E':
          /* no exponent without some digits */
          if ((flags & (NDIGITS | EXPOK)) == EXPOK) {
            flags = (flags & ~(EXPOK | DPTOK)) | SIGNOK | NDIGITS;
            goto fok;
          }
          break;
        }

        ungetc(in, fp);
        break;

      fok:
        *p++ = c;
      }

      /*
       * If no digits, might be missing exponent digits
       * (just give back the exponent) or might be missing
       * regular digits, but had sign and/or decimal point.
       */
      if (flags & NDIGITS) {
        if (flags & EXPOK) {
          /* no digits at all */
          while (p > num_buf)
            ungetc(*--p, fp);
          return nassigned;
        }
        /* just a bad exponent (e and maybe sign) */
        in = *(u_char *)--p;
        if (c != 'e' && c != 'E') {
          ungetc(in, fp); /* sign */
          in = *--p;
        }
        ungetc(in, fp);
      }

      if (!(flags & SUPPRESS)) {
        *p = '\0';
        double res; // = strtod(num_buf, NULL);

        if (flags & LONGDBL)
          tci_throw_error("NoLongDoubleSupport",
                          "TCI doesn't support the `long double` type", 1);
        else if (flags & LONG)
          *va_arg(ap, double *) = res;
        else
          *va_arg(ap, float *) = res;
        nassigned++;
      }

      nread += p - num_buf;
    } break;
    }
  }
}

/*
 * Fill in the given table from the scanset at the given format
 * (just after `[').  Return a pointer to the character past the
 * closing `]'.  The table has a 1 wherever characters should be
 * considered part of the scanset.
 */
static u_char *__sccl(char *tab, u_char *fmt) {
  char c = *fmt++, n, v;

  /* first `clear' the whole table */
  c = *fmt++; /* first char hat => negated scanset */
  if (c == '^') {
    v = 1;      /* default => accept */
    c = *fmt++; /* get new first char */
  } else
    v = 0; /* default => reject */
  /* should probably use memset here */
  for (n = 0; n < 256; n++)
    tab[n] = v;
  if (c == 0)
    return (fmt - 1); /* format ended before closing ] */
  /*
   * Now set the entries corresponding to the actual scanset
   * to the opposite of the above.
   *
   * The first character may be ']' (or '-') without being special;
   * the last character may be '-'.
   */
  v = 1 - v;
  for (;;) {
    tab[c] = v; /* take character c */
  doswitch:
    n = *fmt++; /* and examine the next */
    switch (n) {
    case 0: /* format ended too soon */
      return (fmt - 1);
    case '-':
      /*
       * A scanset of the form
       *	[01+-]
       * is defined as `the digit 0, the digit 1,
       * the character +, the character -', but
       * the effect of a scanset such as
       *	[a-zA-Z0-9]
       * is implementation defined.  The V7 Unix
       * scanf treats `a-z' as `the letters a through
       * z', but treats `a-a' as `the letter a, the
       * character -, and the letter a'.
       *
       * For compatibility, the `-' is not considerd
       * to define a range if the character following
       * it is either a close bracket (required by ANSI)
       * or is not numerically greater than the character
       * we just stored in the table (c).
       */
      n = *fmt;
      if (n == ']' || n < c) {
        c = '-';
        break; /* resume the for(;;) */
      }
      fmt++;
      do { /* fill in the range */
        tab[++c] = v;
      } while (c < n);

      /*
       * Alas, the V7 Unix scanf also treats formats
       * such as [a-c-e] as `the letters a through e'.
       * This too is permitted by the standard....
       */
      goto doswitch;
      break;
    case ']': /* end of scanset */
      return (fmt);
    default: /* just another character */
      c = n;
      break;
    }
  }
  /* NOTREACHED */
}
