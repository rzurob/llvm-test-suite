#include <stdio.h>
#include <limits.h>
#include <float.h>
#include <complex.h>
#include <stdbool.h>
#include <math.h>

// Changes 2007-02-02:
//   pervasive type changes:
//     float _Complex -> complex float
//     double _Complex -> complex double
//     long double _Complex -> complex long double
//   Block with /* Emergency definition ... */" + ifdef removed
//   constants like "1e-5iF", "1e-9iL", and "1e-37i" changed to "1e-5F * I", "1e-9L * I", and "1e-37 * I"


/* C functions to be used in tests of interoperability with FORTRAN
 *
 * Compilation requirements:
 * In XLC, the code must be compiled with -qldbl128 -qlonglong.
 * With gcc, the FORTRAN compile should use -qfloat=complexgcc.
 *
 * INTEGER:
 * This can get tricky, because C defines numeric types in terms of
 * minimum value ranges which may be exceeded, thus "int" must be able
 * to represent numbers in the range -32767 to +32767, but an
 * implementation which could handle -2147483647 to +2147483647 would
 * not be wrong (and, yes, -32767 is not an error, because C is not
 * committed to 2's complement, as opposed to any other scheme, such
 * as signed magnitude, or 1's complement).  Thus, "int" could have
 * the same range as "short int", or "long int", depending on the
 * machine, compiler, etc.
 *
 * We've got two choices:
 * 1. assume we're working with types of a given size (bits or bytes),
 *    i.e., essentially from FORTRAN to C
 * 2. use prescribed ranges, i.e., C to FORTRAN.
 *
 * Since this is a compatibility package which FORTRAN defines to allow
 * C code to be used, we'll let C's ranges dictate.
 *
 * REAL:
 * Again, the ranges are fairly weakly defined, and the number of bytes
 * devoted to the representation is not fixed for all compilers.  
 *
 * COMPLEX:
 * 
 * The issues are similar to those for real, complicated by the fact
 * that there are real and imaginary parts, but also complicated by
 * the fact that a number with an Inf imaginary component has a NaN
 * real component (!).
 * 
 */

int int_fun(int selector);                        /* C_INT:            -32767 to +32767 */
short int short_int_fun(int selector);            /* C_SHORT:          -32767 to +32767 */
long int long_int_fun(int selector);              /* C_LONG:           -2147483647 to +2147483647 */
long long int long_long_int_fun(int selector);    /* C_LONG_LONG:      -9223372036854775807 to +9223372036854775807 */
signed char signed_char_fun(int selector);        /* C_SIGNED_CHAR:    -127 to +127 */
/* we won't bother with unsigned char, since this is the same as C_SIGNED_CHAR to FORTRAN */

float float_inf();
float float_nan();

double double_inf();
double double_nan();

long double long_double_inf();
long double long_double_nan();

float float_fun(int selector);                    /* C_FLOAT:          -1e-37 to +1e37 +/- 1e-5 */
double double_fun(int selector);                  /* C_DOUBLE:         -1e-37 to +1e37 +/- 1e-9 */
long double long_double_fun(int selector);        /* C_LONG_DOUBLE:    -1e-37 to +1e37 +/- 1e-9 */

complex float float_complex_fun(int selector);   /* C_FLOAT_COMPLEX:  */
complex double double_complex_fun(int selector); /* C_DOUBLE_COMPLEX: */
complex long double long_double_complex_fun(int selector); /* C_LONG_DOUBLE_COMPLEX: */

_Bool bool_fun(int selector);                     /* C_BOOL:           false, true */
char char_fun(int selector);                      /* C_CHAR:           */

#define N_VALUES 5

int int_size() { return sizeof(int); }
int short_size() { return sizeof(short int); }
int long_size() { return sizeof(long int); }
int llong_size() { return sizeof(long long int); }
int schar_size() { return sizeof(signed char); }

int float_size() { return sizeof(float); }
int dbl_size() { return sizeof(double); }
int ldbl_size() { return sizeof(long double); }

int cfloat_size() { return sizeof(complex float) / 2; }
int cdbl_size() { return sizeof(complex double) / 2; }
int cldbl_size() { return sizeof(complex long double) / 2; }

int bool_size() { return sizeof(_Bool); }

int char_size() { return sizeof(char); }


signed char signed_char_fun(int selector) {
  signed char vals [N_VALUES] = {-1, 0, 1, 127, -127};
  return (selector < 0 || selector >= N_VALUES) ? 0 : vals[selector];
}

short int short_int_fun(int selector) {
  short int vals [N_VALUES] = {-1, 0, 1, 32767, -32767};
  return (selector < 0 || selector >= N_VALUES) ? 0 : vals[selector];
}

int int_fun(int selector) {
  int vals [N_VALUES] = {-1, 0, 1, 32767, -32767};
  return (selector < 0 || selector >= N_VALUES) ? 0 : vals[selector];
}

long int long_int_fun(int selector) {
  long int vals [N_VALUES] = {-1, 0, 1, 2147483647L, -2147483647L};
  return (selector < 0 || selector >= N_VALUES) ? 0 : vals[selector];
}

long long int long_long_int_fun(int selector) {
  long long int vals [N_VALUES] = {-1, 0, 1, 9223372036854775807LL, -9223372036854775807LL};
  return (selector < 0 || selector >= N_VALUES) ? 0 : vals[selector];
}


float float_fun(int selector) {
  float vals [N_VALUES] = {1e-5F, -1e-37F, +1e37F,  float_nan(), float_inf()}; //* 1.0f/0.0f, FLT_QNAN, FLT_INFINITY
  return (selector < 0 || selector >= N_VALUES) ? 0.0F : vals[selector];
}

double double_fun(int selector) {
double vals [N_VALUES] = {1e-9, -1e-37, +1e37, double_nan(), double_inf()}; //* 1.0/0.0, DBL_QNAN, LDBL_INFINITY
  return (selector < 0 || selector >= N_VALUES) ? 0.0 : vals[selector];
}

long double long_double_fun(int selector) {
  long double vals [N_VALUES] = {1e-9L, -1e-37L, +1e37L, long_double_nan(), long_double_inf()}; //* 1.0L/0.0L, DBL_QNAN, LDBL_INFINITY
  return (selector < 0 || selector >= N_VALUES) ? 0.0L : vals[selector];
}


// This is definitely double-plus-ungood: 0.0 + INFINITY * I becomes (NaNQ,Inf), although that makes no sense to me.
// Also, 0.0 + NAN * I becomes (NaNQ,NaNQ), which makes just as much sense.  This appears to be consistent between
// GCC and XLC, so we'll just have to go with that.

complex float float_complex_fun(int selector){
  //  complex float vals [N_VALUES] = {1e-5F - 1e-5F * _Complex_I, -1e-37F + 1e-37F * _Complex_I, -1e37F - 1e37F * _Complex_I, 0.0F + NAN * _Complex_I, INFINITY + INFINITY *_Complex_I};
  complex float vals [N_VALUES] = {1e-5F - 1e-5F * 1.0Fi, -1e-37F + 1e-37F * 1.0Fi, -1e37F - 1e37F * 1.0Fi, 0.0F + float_nan() * 1.0Fi, float_inf() + float_inf() * 1.0Fi};
  return (selector < 0 || selector >= N_VALUES) ? (0.0F) : vals[selector];
}
complex double double_complex_fun(int selector) {
  complex double vals [N_VALUES] = {1e-9 - 1e-9i, -1e-37 + 1e-37i, -1e37 - 1e37i, 0.0 + double_nan() * 1.0i, double_inf() + double_inf() * 1.0i};
  return (selector < 0 || selector >= N_VALUES) ? (0.0 ) : vals[selector];
}

complex long double long_double_complex_fun(int selector) {
  complex long double vals [N_VALUES] = {1e-9L - 1e-9Li, -1e-37L + 1e-37Li, -1e37L - 1e37Li, 0.0L + long_double_nan() * 1.0i, long_double_inf() + long_double_inf() * 1.0Li};
  return (selector < 0 || selector >= N_VALUES) ? (0.0L) : vals[selector];
}


_Bool bool_fun(int selector) {
  _Bool vals [N_VALUES] = {false, true, !false, !true, false^true};
  return (selector < 0 || selector >= N_VALUES) ? 0 : vals[selector];
}

char char_fun(int selector) {
  char vals [N_VALUES] = {'A', 'z', '\0', '~', '\377'};
  return (selector < 0 || selector >= N_VALUES) ? 0 : vals[selector];
}


// use compiler options "-DDEBUG_VALUES -lm" to produce an executable
// which dumps the values returned, from the C perspective, just to
// verify we're returning the correct values:
#ifdef DEBUG_VALUES
int main(int argc, char **argv) {
  int i;

  printf("int(%d):\t", sizeof(int_fun(0)));
  for (i=0; i<N_VALUES; ++i)
    printf(" %d", int_fun(i));
  printf("\n");

  printf("short_int(%d):\t", sizeof(short_int_fun(0)));
  for (i=0; i<N_VALUES; ++i)
    printf(" %d", short_int_fun(i));
  printf("\n");

  printf("long_int(%d):\t", sizeof(long_int_fun(0)));
  for (i=0; i<N_VALUES; ++i)
    printf(" %ld", long_int_fun(i));
  printf("\n");

  printf("long_long_int(%d):\t", sizeof(long_long_int_fun(0)));
  for (i=0; i<N_VALUES; ++i)
    printf(" %lld", long_long_int_fun(i));
  printf("\n");

  printf("signed_char(%d):\t", sizeof(signed_char_fun(0)));
  for (i=0; i<N_VALUES; ++i)
    printf(" %d", signed_char_fun(i));
  printf("\n");

  printf("long_double(%d):\t", sizeof(long_double_fun(0)));
  for (i=0; i<N_VALUES; ++i)
    printf(" %lg", long_double_fun(i));
  printf("\n");

  printf("double(%d):\t", sizeof(double_fun(0)));
  for (i=0; i<N_VALUES; ++i)
    printf(" %lg", double_fun(i));
  printf("\n");

  printf("float(%d):\t", sizeof(float_fun(0)));
  for (i=0; i<N_VALUES; ++i)
    printf(" %g", float_fun(i));
  printf("\n");

#ifdef CREALL_OKAY
  printf("long_double_complex(%d):\t", sizeof(long_double_complex_fun(0)));
  for (i=0; i<N_VALUES; ++i)
    printf(" (%Lg,%Lg)", creall(long_double_complex_fun(i)), cimagl(long_double_complex_fun(i)));
  printf("\n");
#else
  // This is a hack to get at the real and imaginary values on AIX if creall is not correctly defined - currently a limitation
  printf("long_double_complex(%d):\t", sizeof(long_double_complex_fun(0)));
  for (i=0; i<N_VALUES; ++i)
    printf(" (%llg//0x%llx,%llg//0x%llx)", long_double_complex_fun(i));
  printf("\n");
  for (i=0; i<N_VALUES; ++i)
    printf(" (0x%llx,0x%llx//0x%llx,0x%llx)", long_double_complex_fun(i));
  printf("\n");
#endif

  printf("double_complex(%d):\t", sizeof(double_complex_fun(0)));
  for (i=0; i<N_VALUES; ++i)
    printf(" (%lg,%lg)", creal(double_complex_fun(i)), cimag(double_complex_fun(i)));
  printf("\n");
  printf("double_complex(%d,%d):\t", sizeof(creal(double_complex_fun(0))), sizeof(cimag(double_complex_fun(0))));
  for (i=0; i<N_VALUES; ++i) {
    printf(" (0x%llx,0x%llx)", creal(double_complex_fun(0)), cimag(double_complex_fun(0)));
  }
  printf("\n");

  printf("float_complex(%d):\t", sizeof(float_complex_fun(0)));
  for (i=0; i<N_VALUES; ++i)
    printf(" (%g,%g)", crealf(float_complex_fun(i)), cimagf(float_complex_fun(i)));
  printf("\n");
  printf("float_complex(%d,%d):\t", sizeof(crealf(float_complex_fun(0))), sizeof(cimagf(float_complex_fun(0))));
  for (i=0; i<N_VALUES; ++i) {
    printf(" (0x%x,0x%x)", crealf(float_complex_fun(i)), cimagf(float_complex_fun(i)));
  }
  printf("\n");

  printf("bool(%d):\t", sizeof(bool_fun(0)));
  for (i=0; i<N_VALUES; ++i)
    printf(" %d", bool_fun(i));
  printf("\n");

  printf("char(%d):\t", sizeof(char_fun(0)));
  for (i=0; i<N_VALUES; ++i)
    printf(" %c (%x)", char_fun(i), char_fun(i));
  printf("\n");

}
#endif


/* Types defined (table 15.2, slightly reordered):
 * INTEGER
 *   C_INT                 int
 *   C_SHORT               short int
 *   C_LONG                long int
 *   C_LONG_LONG           long long int
 *   C_SIGNED_CHAR         signed char
 *   C_SIGNED_CHAR         unsigned char
 *
 *   C_INT8_T              int8_t
 *   C_INT16_T             int16_t
 *   C_INT32_T             int32_t
 *   C_INT64_T             int64_t
 *
 *   C_INT_LEAST8_T        int_least8_t
 *   C_INT_LEAST16_T       int_least16_t
 *   C_INT_LEAST32_T       int_least32_t
 *   C_INT_LEAST64_T       int_least64_t
 *
 *   C_INT_FAST8_T         int_fast8_t
 *   C_INT_FAST16_T        int_fast16_t
 *   C_INT_FAST32_T        int_fast32_t
 *   C_INT_FAST64_T        int_fast64_t
 *
 *   C_INTMAX_T            intmax_t
 *
 *   C_INTPTR_T            intptr_t
 *   C_SIZE_T              size_t
 * 
 * REAL
 *   C_FLOAT               float
 *   C_DOUBLE              double
 *   C_LONG_DOUBLE         long double
 * 
 * COMPLEX
 *   C_FLOAT_COMPLEX       complex float
 *   C_DOUBLE_COMPLEX      complex double
 *   C_LONG_DOUBLE_COMPLEX complex long double
 * 
 * LOGICAL
 *   C_BOOL                _Bool
 * 
 * CHARACTER
 *   C_CHAR                char
 *
 */

