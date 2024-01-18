
#include <stdio.h>
#include <stdlib.h>

void csub(int *p, int(**f)(int)) {

   extern int cfun(int);

   int i;

   i = 22;

   if ( *p != 34 ) exit(41);

   *p = (*f)(i);

}

int  cfun(int p) {

   if ( p != 22 ) exit(42); 

   return p;
}

