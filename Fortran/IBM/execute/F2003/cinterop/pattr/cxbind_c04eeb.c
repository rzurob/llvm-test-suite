/* C code for testcase "fxbind_c03eeb.f" */
/* need to use #pragma options ldbl128 for long double */
#pragma options ldbl128 
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>

/* These functions are implemented in Fortran program */

float fn_real4(float *x,float *y) ;
float ent_real4(float *x,float *y);

double fn_real8(double *x,double *y) ;
double ent_real8(double *x,double *y);

long double fn_real16(long double *x,long double *y) ;
long double ent_real16(long double *x,long double *y);

float fn_realval4(float ,float ) ;
float ent_realval4(float ,float );

double fn_realval8(double ,double ) ;
double ent_realval8(double ,double );

long double fn_realval16(long double ,long double ) ;
long double ent_realval16(long double ,long double );

int main()

{
 
  float result_real4,x_real4, y_real4;
  double  result_real8,x_real8, y_real8;

  long double  result_real16,x_real16, y_real16;

  float result_realval4,x_realval4, y_realval4;
  double  result_realval8,x_realval8, y_realval8;

  long double  result_realval16,x_realval16, y_realval16;

  x_real4 = 2.0f;
  y_real4 = 5.0f;

  x_real8 = 2.0;
  y_real8 = 5.0;

  x_real16 = 2.0l;
  y_real16 = 5.0l;

  x_realval4 = 2.0f;
  y_realval4 = 5.0f;

  x_realval8 = 2.0;
  y_realval8 = 5.0;

  x_realval16 = 2.0l;
  y_realval16 = 5.0l;
 
 
 

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

  /* Test 3 */
  result_real16 = fn_real16(&x_real16,&y_real16);
  printf("%f", result_real16);
  assert ( result_real16 == 10.0);
  assert ( x_real16 == 3.0);
  assert ( y_real16 == 6.0);
  result_real16 = ent_real16(&x_real16,&y_real16);

  assert ( result_real16 == 11.0);

  /* Test 4*/
 
  result_realval4 = fn_realval4(x_realval4,y_realval4);
  assert (x_realval4 ==2.0f);
  assert (y_realval4 ==5.0f);
  assert ( result_realval4 == 18.0f);
  result_realval4 = ent_realval4(x_realval4,y_realval4);

  assert ( result_realval4 == 9.0f);

  /* Test 5*/
  result_realval8 = fn_realval8(x_realval8,y_realval8);
  assert (x_realval8 ==2.0);
  assert (y_realval8 ==5.0);
  assert ( result_realval8 == 18.0);
  result_realval8 = ent_realval8(x_realval8,y_realval8);

  assert ( result_realval8 == 9.0);

  /* Test 6*/
  result_realval16 = fn_realval16(x_realval16,y_realval16);
  assert (x_realval16 ==2.0l);
  assert (y_realval16 ==5.0l);
  assert ( result_realval16 == 18.0l);
  result_realval16 = ent_realval16(x_realval16,y_realval16);

  assert ( result_realval16 == 9.0l);

  return 0;
}
