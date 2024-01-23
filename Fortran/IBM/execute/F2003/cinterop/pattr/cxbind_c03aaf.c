/* C code for testcase "fxbind_c03aaf.f" */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

void strrefi(char *a)  
{  
  char zx[11] ="ABCDEFGHIJ";
  static char ax[11] = "abcdefghij" ;  
  int n = 10;
  char *p =a;
  char *q =zx ;
  /* Verify the character array passed from Fortran is correct. */
  while( n-- ) {
    assert (*p++== *q++);
  }

  strncpy ( a, ax, 10 ) ;  
  
}  
