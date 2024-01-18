
#include <stdio.h>
#include <stdlib.h>

void * cfunc(void *p) {

   long long int *a;

   a = malloc(sizeof(long long int));

   if(!(a=malloc(sizeof(long long int)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *a = *(long long int *) p;

   if ( *a != 34 ) exit(41);

   *a = 22;
 
   p = a;

   return a; 

}
