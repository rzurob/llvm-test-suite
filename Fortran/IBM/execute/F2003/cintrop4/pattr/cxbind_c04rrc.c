/* The C code for testcase "fxbind_c04rrc.f"*/
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>
#include <complex.h>

void init_arr_c4(float  _Complex *);
void init_arr_c8(double _Complex *);

/* These functions are implemented in Fortran program */
signed int  maxval_c4(float _Complex  *x, signed int *y) ;
signed int  maxval_c8(double  _Complex  *x, signed int *y) ;

int main() {
  signed int  y;
  signed int  ret_c4,ret_c8 ;
  float _Complex  a_c4[5];
  double _Complex  a_c8[5];
 
  /* Initialization */
  y = 4 ;
  init_arr_c4(a_c4);
  init_arr_c8(a_c8);

  /* Test 2 */
  ret_c4 =  maxval_c4 (&a_c4[0],&y);
  assert (ret_c4 ==4);

  /* Test 3 */
  ret_c8 =  maxval_c8 (&a_c8[0],&y);
  assert (ret_c8 ==4);
  return 0;
}

void init_arr_c4(float  _Complex  *x) {
  int i;
  for ( i = 0; i < 5; i++ ) {
    x[i] = (float)(i+1)+I*(float)(i+1);
  }

}

void init_arr_c8(double  _Complex  *x) {
  int i;
  for ( i = 0; i < 5; i++ ) {
    x[i] = (double)(i+1)+I*(double)(i+1) ;
    x[i] = i+1;
  }

}
