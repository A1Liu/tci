#ifndef __TCI_FLOAT_H
#define __TCI_FLOAT_H

// https://www.rowleydownload.co.uk/arm/documentation/index.htm?https://www.rowleydownload.co.uk/arm/documentation/DBL_MAX.htm

// double exponent minimum and maximum values
#define DBL_MAX_10_EXP 308U
#define DBL_MAX_EXP 1024
#define DBL_MIN_10_EXP -307
#define DBL_MIN_EXP -1021

// implementation
#define DBL_DIG 15
#define DBL_MANT_DIG 53
#define DECIMAL_DIG 17
#define FLT_DIG 6
#define FLT_EVAL_METHOD 0
#define FLT_MANT_DIG 24
#define FLT_RADIX 2
#define FLT_ROUNDS 1

// float exponent minimum and maximum values
#define FLT_MAX_10_EXP 38
#define FLT_MAX_EXP +128
#define FLT_MIN_10_EXP -37
#define FLT_MIN_EXP -125

// double minimum and maximum values
#define DBL_EPSILON 2.2204460492503131E-16
#define DBL_MAX 1.7976931348623157E308
#define DBL_MIN 2.2250738585072014E-308

// float minimum and maximum values
#define FLT_EPSILON 1.19209290E-07F
#define FLT_MAX 3.40282347E38F
#define FLT_MIN 1.17549435E-38F

#endif /* __TCI_FLOAT_H */
