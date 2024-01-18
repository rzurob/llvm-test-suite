
#include <stdio.h>
#include <stdlib.h>

void csub1(int *p) {

   if ( *p != 1 ) exit(41);

   *p = 10;

}

void csub2(int *p) {

   if ( *p != 2 ) exit(42);

   *p = 20;

}

