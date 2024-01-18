
#include <stdio.h>
#include <stdlib.h>

void csub(int *p, void (**f)(int * , int *)) {

   int a;
   int b;

   a = 14;
   b = 8;

   if ( *p != 34 ) exit(41);
   if( a != 14 ) exit(42);
   if( b != 8 ) exit(43);

   (*f)(&a, &b); 

   *p = 22;

   if( a != 8 ) exit(44);
   if( b != 14 ) exit(45);

}

void  swap(int *p1, int *p2) {

   int tmp;

   if (*p1 != 14 ) exit(46);  
   if (*p2 != 8 ) exit(47);

   tmp = *p2;
   *p2 = *p1;
   *p1 = tmp;

}

