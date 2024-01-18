
#include <stdio.h>
#include <stdlib.h>

void * cfunc(void *p) {

   short int *a;

   a = malloc(sizeof(short int));

   if(!(a=malloc(sizeof(short int)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *a = *(short int *) p;

   if ( *a != 34 ) exit(41);

   *a = 22;
 
   p = a;

   return a; 

}
