
#include <stdio.h>
#include <stdlib.h>

void csub(void(**f)(_Bool *, _Bool *)) {

   extern void swap(_Bool *, _Bool *);

   *f = &swap;

}

void  swap(_Bool *p1, _Bool *p2) {

   _Bool tmp;

   if (*p1 != 1 ) exit(46);
   if (*p2 != 0 ) exit(47);

   tmp = *p2;
   *p2 = *p1;
   *p1 = tmp;

}

