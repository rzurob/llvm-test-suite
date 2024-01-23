/* C code for testcase "fxbind_c03egc.f" */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>

/* These functions are implemented in Fortran program */

unsigned char fn_log1(unsigned char *);
unsigned char fn_logval1(unsigned char );

int main()

{
  unsigned char result_log1,x_log1, y_log1;
  unsigned char result_logval1,x_logval1, y_logval1;
  

  /* Test 1 */

  x_log1 = 1;
  result_log1 =  fn_log1(&x_log1);
  assert ( x_log1 == 0 ) ;
  assert (  result_log1 == 0 ) ;

  result_log1 =  ent_log1();
  assert (  result_log1 == 1 ) ;

  /* Test 2 */

  x_logval1 = 1;
  result_logval1 =  fn_logval1(x_logval1);
  assert ( x_logval1 == 1 ) ;
  assert (  result_logval1 == 0 ) ;
  result_logval1 =  ent_logval1();
  assert (  result_logval1 == 1 ) ;

  return 0;
}
