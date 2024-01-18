/* C code for testcase "fxbind_c04egc.f" */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>

/* These functions are implemented in Fortran program */

unsigned char ent_log1();
unsigned char ent_logval1();

int main()

{
unsigned char result_log1,x_log1, y_log1;
unsigned char result_logval1,x_logval1, y_logval1;

/* Test 1 */
result_log1 =  ent_log1();
assert (  result_log1 == 1 ) ;

/* Test 2 */
result_logval1 =  ent_logval1();
assert (  result_logval1 == 1 ) ;

return 0;
}
