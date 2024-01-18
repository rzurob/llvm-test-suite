#include <stdio.h>
#include <math.h>
#include <limits.h>
#include <float.h>
#include <complex.h>
#include <stdbool.h>

/* Service routines and data for acemc05 and acemc15.
 *
 * Compilation requirements:
 * In XLC, the code must be compiled with -qldbl128 -qlonglong.
 * With gcc, the FORTRAN compile should use -qfloat=complexgcc.
 *
 */

#define MAX_ITEMS 5

float float_inf();
float float_nan();

double double_inf();
double double_nan();

long double long_double_inf();
long double long_double_nan();


/* typedefs */

typedef struct {
  complex long double long_double_complex_field;
  complex double double_complex_field;
  complex float float_complex_field;
} complex_stuff;

typedef struct {
  long double long_double_field;
  double double_field;
  float float_field;
} fp_stuff;

typedef struct {
  long long int long_long_field;
  long int long_field;
  int int_field;
  short int short_field;
  signed char signed_char_field;
} int_stuff;

typedef struct {
  complex_stuff complex_stuff_field;
  fp_stuff fp_stuff_field;
  int_stuff int_stuff_field;
  _Bool bool_field;
  char char_field;
} foreign;


/* Data definitions */

complex_stuff c_array[] = {{1e1L + 2e2Li, 3.0 + 4.0i, 0.5f + 0.6fi},
			   {-1.0L + 2.0Li, 3.0 - 4.0i, -5.0f - 6.0fi},
			   {1e200L + 2e-200Li, 3.0e37 + 4.0e-37i, 0.5e10f + 0.6e-10fi},
			   {0,0,0}, // to be filled in by init_arrays with: {NAN + NAN * I, NAN + NAN * I, NAN + NAN * I},
			   {0,0,0}}; // to be filled in by init_arrays with: {NAN + INFINITY * I, NAN + INFINITY * I, NAN + INFINITY * I}

// Yes, (NaN,Inf) is crazy, but talk to the C people about that - see my note in cfuns.c in this directory.

fp_stuff fp_array[] = {{-9e-100L,-8.0e30,-7.0e20F},
		       {-0.0L, -0.0, -0.0F},
		       {0,0,0}, // to be filled in by init_arrays with: {INFINITY, INFINITY, INFINITY}
		       {9.9e200L, 2.1e-30, 0.3e-9F},
		       {0, 0, 0}}; // to be filled in by init_arrays with: {NAN, NAN, NAN}

int_stuff int_array [] ={{9223372036854775807LL, 2147483647L, 32767, 32767, 127},
			 {-9223372036854775807LL, -2147483647L, -32767, -32767, -127},
			 {-1LL, -1L, -1, -1, -1},
			 { 0LL,  0L,  0,  0,  0},
			 { 1LL,  1L,  1,  1,  1}};

foreign foreign_array [MAX_ITEMS];


/* Access/manipulation functions:
 * init_arrays - should be invoked before anything else, to initialise the arrays.
 * get_<thing> - gets the inx'th item from the <thing> array defined above;
 *               note that indexing is FORTRAN-style: starting at 1, not 0
 * record_<thing> - copies the argument array to the <thing> array defined above;
 * In all of the above, indexes must be whole numbers not exceeding MAX_ITEMS.
 */

void init_arrays() {
  fp_stuff inf_f_item = {long_double_inf(), double_inf(), float_inf()};
  fp_stuff nan_f_item = {long_double_nan(), double_nan(), float_nan()};
  complex_stuff inf_z_item = {0.0 /* long_double_nan() + long_double_inf() * 1.0Li*/, double_nan() + double_inf() * 1.0i, float_nan() + float_inf() * 1.0fi};
  complex_stuff nan_z_item = {long_double_nan() + long_double_nan() * 1.0Li, double_nan() + double_nan() * 1.0i, float_nan() + float_nan() * 1.0fi};
  int i;
  complex long double var;

  // Yes, this is an awful hack, but there doesn't seem to be another way to reliably initialise this complex number:
  __real__(var) = long_double_nan();
  __imag__(var) = long_double_inf();

  inf_z_item.long_double_complex_field = var;

  fp_array[2] = inf_f_item;
  fp_array[4] = nan_f_item;
  c_array[3] = inf_z_item;
  c_array[4] = nan_z_item;
  for (i = 0; i < MAX_ITEMS; ++i) {
    foreign_array[i].complex_stuff_field = c_array[i];
    foreign_array[i].fp_stuff_field = fp_array[i];
    foreign_array[i].int_stuff_field = int_array[i];
    foreign_array[i].bool_field = ((i%2) == 0);
    foreign_array[i].char_field = ('a' + i);
  }
}

complex_stuff get_complex_stuff (int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? c_array[inx-1] : c_array[0];
}

void record_complex_stuff(int len, complex_stuff incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    c_array [inx] = incoming[inx];
}


fp_stuff get_fp_stuff(int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? fp_array[inx-1] : fp_array[0];
}

void record_fp_stuff(int len, fp_stuff incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    fp_array [inx] = incoming[inx];
}


int_stuff get_int_stuff(int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? int_array[inx-1] : int_array[0];
}

void record_int_stuff(int len, int_stuff incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    int_array [inx] = incoming[inx];
}


foreign get_foreign(int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? foreign_array[inx-1] : foreign_array[0];
}

void record_foreign(int len, foreign incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    foreign_array [inx] = incoming[inx];
}


#ifdef DEBUG_TEST

void main (int argc, char **argv) {
  fp_stuff fp_item;
  complex_stuff z_item;
  int_stuff i_item;
  //  foreign f_item;
  int i;

  init_arrays();

  printf("fp stuff:\n");
  for (i=1; i <= MAX_ITEMS; ++i) {
    fp_item = get_fp_stuff(i);
    printf("%d: {%lle, %le, %e}\n",i, fp_item.long_double_field, fp_item.double_field, fp_item.float_field);
  }

  printf("int stuff:\n");
  for (i=1; i <= MAX_ITEMS; ++i) {
    i_item = get_int_stuff(i);
    printf("%d: {%lld, %ld, %d, %d}\n",i, i_item.long_long_field, i_item.long_field, i_item.int_field, (int) i_item.short_field, (int) i_item.signed_char_field);
  }

  printf("complex stuff:\n");
  for (i=1; i <= MAX_ITEMS; ++i) {
    z_item = get_complex_stuff(i);
    printf("%d: {(%llg,%llg), (%lg,%lg), (%g,%g)}\n",i, creal(z_item.long_double_complex_field), cimag(z_item.long_double_complex_field),
	   creal(z_item.double_complex_field), cimag(z_item.double_complex_field), creal(z_item.float_complex_field), cimag(z_item.float_complex_field));
  }

}
#endif
