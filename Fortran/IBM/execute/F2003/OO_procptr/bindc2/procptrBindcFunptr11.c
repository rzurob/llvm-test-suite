
#include <stdio.h>
#include <stdlib.h>

void csub(void(**f)(short int *, short int *)) {

   extern void swap(short int *, short int *);

   *f = &swap;

}

void  swap(short int *p1, short int *p2) {

   int tmp;

   if (*p1 != 10 ) exit(46);
   if (*p2 != 20 ) exit(47);

   tmp = *p2;
   *p2 = *p1;
   *p1 = tmp;

}

