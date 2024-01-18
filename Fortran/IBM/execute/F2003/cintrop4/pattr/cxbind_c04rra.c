/* The C code for testcase "fxbind_c04rra.f"*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>

void init_arr_i2(signed short * );
void init_arr_i4(signed int * );
void init_arr_i8(signed long long  * );

/* These functions are implemented in Fortran program */
signed short  maxval_i2(signed short *x, signed int * y) ;
signed int  maxval_i4(signed int *x, signed int * y) ;
signed long long  maxval_i8(signed long long  *x, signed int * y) ;

int main() {
  signed int  y;
  signed short a_i2[5],ret_i2;
  signed int a_i4[5],ret_i4;
  signed long long  a_i8[5],ret_i8;
  /* Initialization */
  y = 5 ;
  init_arr_i2(a_i2);
  init_arr_i4(a_i4);
  init_arr_i8(a_i8);

  /* Test 1 */
  ret_i2 =  maxval_i2 (&a_i2[0],&y);
  printf ("The max value is %d",ret_i2);
  assert (ret_i2 ==5);

  /* Test 2 */
  ret_i4 =  maxval_i4 (&a_i4[0],&y);
  assert (ret_i4 ==5);

  /* Test 3 */
  ret_i8 =  maxval_i8 (&a_i8[0],&y);
  assert (ret_i8 ==5l);

  return 0;
}

void init_arr_i2(signed short *x) {
  signed short i;

  for ( i = 0; i < 5; i++ ) {
    x[i] = i+1;
  }
 
  printf ("%d",x[1]);
}

void init_arr_i4(signed int  *x) {
  int i;

  for ( i = 0; i < 5; i++ ) {
    x[i] = i+1;
  }

}

void init_arr_i8(signed long long  *x) {
  signed long long  i;

  for ( i = 0; i < 5; i++ ) {
    x[i] = i+1;
  }

}
