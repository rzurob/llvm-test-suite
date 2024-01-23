/* C code for testcase "fxbind_c03aac.f" */

#include <stdio.h>
#include <math.h>
#include <assert.h>

char fun_char_ref( char *x, char *y) {
  assert ( *x == 'A' ) ;
  assert ( *y == 'B') ;

  *x = 'C';
  *y = 'D';
  return ('E');
}

char fun_char_val(char x, char y) {
  assert ( x == 'A' ) ;
  assert ( y == 'B') ;
  x = 'C';
  y = 'D';
  return ('E');
}

unsigned char exfun_log_ref1(unsigned char *i, unsigned char *l){
  *i = *l;
  return (*i);
}

unsigned char exfun_log_val1(unsigned char i, unsigned char l){
  i = l;
  return (i);
}

unsigned char retlog( unsigned char *r )  
{  
  unsigned char s;  
  s = *r;  
  if ( s == 0 ) s = 1 ; else s = 0 ;  
  return ( s );  
}  
