
#include <stdio.h>
#include <stdlib.h>

void * cfunc(void *p) {

   long double *a;

   a = malloc(sizeof(long double));

   if(!(a=malloc(sizeof(long double)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *a = * (long double *) p;

   if ( *a != 5.0f ) exit(41);

   *a = 10.0f;
 
   p = a;

   return a; 

}
