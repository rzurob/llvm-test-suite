
#include <stdio.h>
#include <stdlib.h>

void csub(void(**f)(float *, float *)) {

   extern void swap(float *, float *);

   *f = &swap;

}

void  swap(float *p1, float *p2) {

   float tmp;

   if (*p1 != 5.0f ) exit(46);
   if (*p2 != 25.0f ) exit(47);

   tmp = *p2;
   *p2 = *p1;
   *p1 = tmp;

}

