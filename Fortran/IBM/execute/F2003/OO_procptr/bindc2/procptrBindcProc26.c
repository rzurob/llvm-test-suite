
#include <stdio.h>
#include <stdlib.h>

void * cfunc(void *p) {

   _Bool *a;

   a = malloc(sizeof(_Bool));

   if(!(a=malloc(sizeof(_Bool)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *a = * (_Bool *) p;

   if ( *a != 1 ) exit(41);

   *a = 0;
 
   p = a;

   return a; 

}
