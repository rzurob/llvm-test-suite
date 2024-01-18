
#include <stdio.h>
#include <stdlib.h>

void * cfunc(void *p) {

   signed char *a;

   a = malloc(sizeof(signed char));

   if(!(a=malloc(sizeof(signed char)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *a = * (signed char *) p;

   if ( *a != 'A' ) exit(41);

   *a = 'C';
 
   p = a;

   return a; 

}
