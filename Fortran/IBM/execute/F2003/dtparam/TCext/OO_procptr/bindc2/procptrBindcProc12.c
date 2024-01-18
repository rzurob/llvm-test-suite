#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void * cfunc(void *p) {

   int_least32_t *a;

   a = malloc(sizeof(int_least32_t));

   if(!(a=malloc(sizeof(int_least32_t)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *a = *(int_least32_t *) p;

   if ( *a != 34 ) exit(41);

   *a = 22;
 
   p = a;

   return a; 

}
