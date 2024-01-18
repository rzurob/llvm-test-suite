
#include <stdio.h>
#include <stdlib.h>

void * cfunc(void *p) {

   double *a;

   a = malloc(sizeof(double));

   if(!(a=malloc(sizeof(double)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *a = * (double *) p;

   if ( *a != 5.0f ) exit(41);

   *a = 10.0f;
 
   p = a;

   return a; 

}
