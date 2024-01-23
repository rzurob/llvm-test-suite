/* C code for testcase "fxbind_c03egb.f" */
/* need to use #pragma options ldbl128 for long double */
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>

/* These functions are implemented in Fortran program */

float fn_real4(float *x,float *y) ;
float ent_real4(float *x,float *y);

double fn_real8(double *x,double *y) ;
double ent_real8(double *x,double *y);

float fn_realval4(float ,float ) ;
float ent_realval4(float ,float );

double fn_realval8(double ,double ) ;
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
  result_real4 = fn_real4(&x_real4,&y_real4);
  assert ( result_real4 == 10.0f);
  assert ( x_real4 == 3.0f);
  assert ( y_real4 == 6.0f);
  result_real4 = ent_real4(&x_real4,&y_real4);

  assert ( result_real4 == 11.0f);

  /* Test 2 */
  result_real8 = fn_real8(&x_real8,&y_real8);
  assert ( result_real8 == 10.0);
  assert ( x_real8 == 3.0);
  assert ( y_real8 == 6.0);
  result_real8 = ent_real8(&x_real8,&y_real8);

  assert ( result_real8 == 11.0);
  
  /* Test 3*/
 
  result_realval4 = fn_realval4(x_realval4,y_realval4);
  assert (x_realval4 ==2.0f);
  assert (y_realval4 ==5.0f);
  assert ( result_realval4 == 18.0f);
  result_realval4 = ent_realval4(x_realval4,y_realval4);

  assert ( result_realval4 == 9.0f);

  /* Test 4*/
  result_realval8 = fn_realval8(x_realval8,y_realval8);
  assert (x_realval8 ==2.0);
  assert (y_realval8 ==5.0);
  assert ( result_realval8 == 18.0);
  result_realval8 = ent_realval8(x_realval8,y_realval8);

  assert ( result_realval8 == 9.0);
  printf("%s", "The testcase fxbind_c03egb.f is run successfully");
  return 0;
}
