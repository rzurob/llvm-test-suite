
#include <stdio.h>
#include <stdlib.h>

struct dt0 {
   int a;
};

struct dt1 {
   int b;
   struct dt0 d0;
};

struct dt2 {
   int c;
   struct dt1 d1;
};


void * cfunc(void *p) {

   int i;
   struct dt2 *q;

   q = malloc(sizeof(struct dt2));

   if(!(q=malloc(sizeof(struct dt2)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *q = * (struct dt2 *) p;

   if((q)->c != 1) exit(41);
   if((q)->d1.b != 2) exit(42);
   if((q)->d1.d0.a != 3) exit(43);

   (q)->c = 11;
   (q)->d1.b = 22;
   (q)->d1.d0.a = 33;

   p = q;

   return q; 

}
