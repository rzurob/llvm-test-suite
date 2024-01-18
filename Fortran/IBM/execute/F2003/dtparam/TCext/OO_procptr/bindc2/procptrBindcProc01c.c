
#include <stdio.h>
#include <stdlib.h>

void * cfunc(void **p) {

   int *a;

   a = malloc(sizeof(int));

   if(!(a=malloc(sizeof(int)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *a = *(int *) *p;

   if ( *a != 34 ) exit(41);

   *a = 22;

   *p = a;

   return a; 

}
