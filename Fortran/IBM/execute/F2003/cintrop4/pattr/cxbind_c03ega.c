/* C code for testcase "fxbind_c03ega.f" */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>

/* These functions are implemented in Fortran program */
signed char ent_int1(signed char *x,signed char *y);

signed short ent_int2(signed short *x,signed short *y);

signed int ent_int4(signed int *x,signed int *y);

signed long long ent_int8(signed long long *x,signed long long *y);

signed char ent_intval1(signed char ,signed char );

signed short ent_intval2(signed short ,signed short );

signed int ent_intval4(signed int ,signed int );

signed long long ent_intval8(signed long long ,signed long long );

int main()

{
  signed char result_int1,x_int1, y_int1;
  signed short result_int2,x_int2, y_int2;
  signed int result_int4,x_int4, y_int4;
  signed long long  result_int8,x_int8, y_int8;

  signed char result_intval1,x_intval1, y_intval1;
  signed short result_intval2,x_intval2, y_intval2;
  signed int result_intval4,x_intval4, y_intval4;
  signed long long  result_intval8,x_intval8, y_intval8;

  x_int1 = 2;
  y_int1 = 5;

  x_int2 = 2;
  y_int2 = 5;

  x_int4 = 2;
  y_int4 = 5;

  x_int8 = 2;
  y_int8 = 5;

  x_intval1 = 2;
  y_intval1 = 5;

  x_intval2 = 2;
  y_intval2 = 5;

  x_intval4 = 2;
  y_intval4 = 5;

  x_intval8 = 2;
  y_intval8 = 5;

  /* Test 1 */
  
  result_int1 = ent_int1(&x_int1,&y_int1);

  assert ( result_int1 == 9);
  
  
  /* Test 2 */
  
  result_int2 = ent_int2(&x_int2,&y_int2);

  assert ( result_int2 == 9);

  /* Test 3 */
 
  result_int4 = ent_int4(&x_int4,&y_int4);

  assert ( result_int4 == 9);

  /* Test 4 */
  
  result_int8 = ent_int8(&x_int8,&y_int8);

  assert ( result_int8 == 9);

  /* Test 5 */
 
  result_intval1 = ent_intval1(x_intval1,y_intval1);

  assert ( result_intval1 == 9);

  /* Test 6*/
 
  result_intval2 = ent_intval2(x_intval2,y_intval2);

  assert ( result_intval2 == 9);

  /* Test 7*/
  
  result_intval4 = ent_intval4(x_intval4,y_intval4);

  assert ( result_intval4 == 9);

  /* Test 8*/
  
  result_intval8 = ent_intval8(x_intval8,y_intval8);

  assert ( result_intval8 == 9);

  return 0;
}
