
#include <stdio.h>
#include <stdlib.h>

void csub(int *p1, int *p2, int *p3, int(**f)(int)) {

   int i;

   i = 11;

   if ( *p1 != 1 ) exit(41);
   if ( *p2 != 2 ) exit(42);
   if ( *p3 != 3 ) exit(43);

   *p1 = (*f[0])(i);
   *p2 = (*f[1])(i+1);
   *p3 = (*f[2])(i+2);


}

int  cfun1(int p) {

   if ( p != 11 ) exit(44); 

   return p;
}

int  cfun2(int p) {

   if ( p != 12 ) exit(45);

   return p;
}

int  cfun3(int p) {

   if ( p != 13 ) exit(46);

   return p;
}
