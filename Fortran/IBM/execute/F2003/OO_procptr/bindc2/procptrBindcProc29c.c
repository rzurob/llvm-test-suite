
#include <stdio.h>
#include <stdlib.h>

struct dt {
   int a[5];
};

void * cfunc(void **p) {

   int i;
   struct dt *q;

   q = malloc(sizeof(struct dt));

   if(!(q=malloc(sizeof(struct dt)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *q = * (struct dt *) *p;

   for ( i = 0; i < 5; i++ ) {
     if ( (q)->a[i] != 2 ) exit(42);
   }

   for ( i = 0; i < 5; i++ ) {
          (q)->a[i] = 10;
   }

   *p = q;

   return q; 

}
