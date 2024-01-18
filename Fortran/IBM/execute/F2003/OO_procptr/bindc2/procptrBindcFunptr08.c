
#include <stdio.h>
#include <stdlib.h>

void csub(int(**f)(int)) {

   extern int cfun(int);

    *f = &cfun;

}

int  cfun(int p) {

   if ( p != 22 ) exit(42); 

   return p;
}

