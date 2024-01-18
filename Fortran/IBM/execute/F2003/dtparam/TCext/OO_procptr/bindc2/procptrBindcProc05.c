
#include <stdio.h>
#include <stdlib.h>

void * cfunc(void *p) {

   size_t *a;

   a = malloc(sizeof(size_t));

   if(!(a=malloc(sizeof(size_t)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *a = *(size_t *) p;

   if ( *a != 34 ) exit(41);

   *a = 22;
 
   p = a;

   return a; 

}
