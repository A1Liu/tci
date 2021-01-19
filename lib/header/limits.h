/*
 * Copyright (c) 2000 Apple Computer, Inc. All rights reserved.
 *
 * @APPLE_LICENSE_HEADER_START@
 *
 * The contents of this file constitute Original Code as defined in and
 * are subject to the Apple Public Source License Version 1.1 (the
 * "License").  You may not use this file except in compliance with the
 * License.  Please obtain a copy of the License at
 * http://www.apple.com/publicsource and read it before using this file.
 *
 * This Original Code and all software distributed under the License are
 * distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT.  Please see the
 * License for the specific language governing rights and limitations
 * under the License.
 *
 * @APPLE_LICENSE_HEADER_END@
 */
#ifndef __TCI_LIMITS_H
#define __TCI_LIMITS_H

/* Number of bits in a `char'.  */
#define CHAR_BIT 8

/* Maximum length of a multibyte character.  */
#define MB_LEN_MAX 1

/* Minimum and maximum values a `signed char' can hold.  */
#define SCHAR_MIN (-128)
#define SCHAR_MAX 127

/* Maximum value an `unsigned char' can hold.  (Minimum is 0).  */
#define UCHAR_MAX 255

/* Minimum and maximum values a `char' can hold.  */
#define CHAR_MIN (-128)
#define CHAR_MAX 127

/* Minimum and maximum values a `signed short int' can hold.  */
#define SHRT_MIN (-32768)
#define SHRT_MAX 32767

/* Maximum value an `unsigned short int' can hold.  (Minimum is 0).  */
#define USHRT_MAX 65535

/* Minimum and maximum values a `signed int' can hold.  */
#define __INT_MAX__ 2147483647
#define INT_MIN (-INT_MAX - 1)
#define INT_MAX __INT_MAX__

/* Maximum value an `unsigned int' can hold.  (Minimum is 0).  */
#define UINT_MAX (INT_MAX * 2U + 1)

/* Minimum and maximum values a `signed long int' can hold.
   (Same as `int').  */
#define __LONG_MAX__ 2147483647L
#define LONG_MIN (-LONG_MAX - 1)
#define LONG_MAX __LONG_MAX__

/* Maximum value an `unsigned long int' can hold.  (Minimum is 0).  */
#define ULONG_MAX (LONG_MAX * 2UL + 1)

/* Minimum and maximum values a `signed long long int' can hold.  */
#define __LONG_LONG_MAX__ 9223372036854775807LL
#define LONG_LONG_MIN (-LONG_LONG_MAX - 1)
#define LONG_LONG_MAX __LONG_LONG_MAX__

/* Maximum value an `unsigned long long int' can hold.  (Minimum is 0).  */
#define ULONG_LONG_MAX (LONG_LONG_MAX * 2ULL + 1)

#endif /* _LIMITS_H___ */
