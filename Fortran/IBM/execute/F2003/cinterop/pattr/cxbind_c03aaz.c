/* C code for testcase "fxbind_c03aaz.f" */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/* These functions are implemented in Fortran program */

char fn_char( char *x, char y);
char ent_char();
 
int main()
{
  char x,y,result_fn,result_ent;
  x='M';
  y='N';  

  /* Test 1 */
  result_fn = fn_char(&x, y);
  assert ( result_fn == 'N');
 
  /* Test 2 */
  result_ent = ent_char();
  assert ( result_ent == 'K'); 

  return 0;
}
