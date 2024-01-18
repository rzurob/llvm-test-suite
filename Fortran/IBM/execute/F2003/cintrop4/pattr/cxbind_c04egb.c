/* C code for testcase "fxbind_c04egb.f" */
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>

/* These functions are implemented in Fortran program */

float ent_real4(float *x,float *y);
double ent_real8(double *x,double *y);

float ent_realval4(float ,float );
double ent_realval8(double ,double );

int main()

{
 
  float result_real4,x_real4, y_real4;
  double  result_real8,x_real8, y_real8;

  float result_realval4,x_realval4, y_realval4;
  double  result_realval8,x_realval8, y_realval8;

  x_real4 = 2.0f;
  y_real4 = 5.0f;

  x_real8 = 2.0;
  y_real8 = 5.0;

  x_realval4 = 2.0f;
  y_realval4 = 5.0f;

  x_realval8 = 2.0;
  y_realval8 = 5.0;
 
  /* Test 1 */
  result_real4 = ent_real4(&x_real4,&y_real4);
  assert ( result_real4 == 9.0f);

  /* Test 2 */
  result_real8 = ent_real8(&x_real8,&y_real8);
  assert ( result_real8 == 9.0);
 
  /* Test 3*/
  result_realval4 = ent_realval4(x_realval4,y_realval4);
  assert ( result_realval4 == 9.0f);

  /* Test 4*/
  result_realval8 = ent_realval8(x_realval8,y_realval8);
  assert ( result_realval8 == 9.0);

  printf("%s", "The testcase is run successfully");
  return 0;
}
