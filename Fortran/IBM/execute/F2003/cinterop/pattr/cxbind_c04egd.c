/* C code for testcase "fxbind_c04egd.f" */

#include <stdio.h>
#include <complex.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

/* These functions are implemented in Fortran program */

float _Complex swap_c4(float _Complex *x, float _Complex *y);
double _Complex swap_c8(double _Complex *x,double _Complex *y) ;

float enswap_c4(float _Complex *x, float _Complex *y);
float enswap_c8(double _Complex *x,double _Complex *y) ;

float _Complex swapval_c4(float _Complex , float _Complex );
double _Complex swapval_c8(double _Complex ,double _Complex ) ;

float enswapval_c4(float _Complex , float _Complex );
float enswapval_c8(double _Complex ,double _Complex y) ;

int main()
{
  float _Complex x_c4,y_c4,res_c4;
  float ren;
  double _Complex x_c8,y_c8,res_c8;
   
  x_c4 = 5.0f+I*5.0f;
  y_c4 = 10.0f+I*10.0f;

  x_c8 = 5.0+I*5.0;   
  y_c8 = 10.0+I*10.0;


  /* Test 1 */
  
  ren =  enswap_c4(&x_c4,&y_c4);
  assert (ren==5.0f);

  /* Test 2 */
  ren =  enswap_c8(&x_c8,&y_c8);
  assert (ren==5.0);
 
  /* Re-initialize the variables */
  x_c4 = 5.0f+I*5.0f;
  y_c4 = 10.0f+I*10.0f;

  x_c8 = 5.0+I*5.0;
  y_c8 = 10.0+I*10.0;

   /* Test 3 */
  ren =  enswapval_c4(x_c4,y_c4);
  assert (ren==5.0f);

  /* Test 4 */
  ren =  enswapval_c8(x_c8,y_c8);
  assert (ren==5.0);

}
