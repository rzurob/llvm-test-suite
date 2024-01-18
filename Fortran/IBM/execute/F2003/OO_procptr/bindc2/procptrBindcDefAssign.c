
#include <stdio.h>
#include <stdlib.h>


void csub(char(**f)(char)) {

   extern char cfun(char);

    *f = &cfun;

}

char  cfun(char p) {

   if ( p != 'A' ) exit(42);

   p = 'B';

   return p;
}

