
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

void * cfunc(void *p) {

   int64_t *a;

   a = malloc(sizeof(int64_t));

   if(!(a=malloc(sizeof(int64_t)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *a = *(int64_t *) p;

   if ( *a != 34 ) exit(41);

   *a = 22;
 
   p = a;

   return a; 

}
