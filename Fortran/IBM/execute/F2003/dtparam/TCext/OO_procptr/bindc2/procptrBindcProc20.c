
#include <stdio.h>
#include <stdlib.h>

void * cfunc(void *p) {

   float *a;

   a = malloc(sizeof(float));

   if(!(a=malloc(sizeof(float)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *a = * (float *) p;

   if ( *a != 5.0f ) exit(41);

   *a = 10.0f;
 
   p = a;

   return a; 

}
