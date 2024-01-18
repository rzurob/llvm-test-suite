#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void * cfunc(void *p) {

   intptr_t *a;

   a = malloc(sizeof(intptr_t));

   if(!(a=malloc(sizeof(intptr_t)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *a = *(intptr_t *) p;

   if ( *a != 34 ) exit(41);

   *a = 22;
 
   p = a;

   return a; 

}
