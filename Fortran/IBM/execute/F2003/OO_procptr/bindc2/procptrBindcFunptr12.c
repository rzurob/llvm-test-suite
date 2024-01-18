
#include <stdio.h>
#include <stdlib.h>

void csub(void(**f)(long int *, long int *)) {

   extern void swap(long int *, long int *);

   *f = &swap;

}

void  swap(long int *p1, long int *p2) {

   int tmp;

   if (*p1 != 10 ) exit(46);
   if (*p2 != 20 ) exit(47);

   tmp = *p2;
   *p2 = *p1;
   *p1 = tmp;

}

