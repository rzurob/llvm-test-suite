
#include <stdio.h>
#include <stdlib.h>

void * cfunc(void *p) {

   char *a;

   a = malloc(sizeof(char));

   if(!(a=malloc(sizeof(char)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *a = * (char *) p;

   if ( *a != 'A' ) exit(41);

   *a = 'C';
 
   p = a;

   return a; 

}
