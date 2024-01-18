
#include <stdio.h>
#include <stdlib.h>

void csub(char *p, char(**f)(char)) {

   extern char cfun(char);

   char i;

   i = 'A';

   if ( *p != 'B' ) exit(41);

   *p = (*f)(i);

}

char  cfun(char p) {

   if ( p != 'A' ) exit(42); 

   return p;
}

