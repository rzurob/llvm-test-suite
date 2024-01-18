/* The C code for testcase "fxbind_c04rrb.f"*/
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>

void init_arr_r4(float * );
void init_arr_r8(double  * );

/* These functions are implemented in Fortran program */
float  maxval_r4(float *x, signed int *y) ;
double  maxval_r8(double  *x, signed int *y) ;

int main() {
  signed int  y;
  float a_r4[5],ret_r4;
  double  a_r8[5],ret_r8;
  /* Initialization */
  y = 5 ;
  init_arr_r4(a_r4);
  init_arr_r8(a_r8);

  /* Test 1 */
  ret_r8 =  maxval_r8 (&a_r8[0],&y);
  assert (ret_r8 ==5.0);

  /* Test 2 */
  ret_r4 =  maxval_r4 (&a_r4[0],&y);
  assert (ret_r4 ==5.0f);

  printf("%s"," The testcase fxbind_c03rrb.f is run successfully.");
  return 0;
}

void init_arr_r4(float  *x) {
  int i;

  for ( i = 0; i < 5; i++ ) {
    x[i] = (float)(i+1);
  }

}

void init_arr_r8(double  *x) {
  int i;

  for ( i = 0; i < 5; i++ ) {
    x[i] = (double)(i+1);
    x[i] = i+1;
  }

}
