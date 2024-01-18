#include <stdio.h>
#include <stdlib.h>
  
  int use_int(void *);

  int greeting(int (*)(void *), void *);
  
  int greeting(int (*fp)(void *), void *q) {

    int *p;

    p = malloc(sizeof(int));

    if(!(p=malloc(sizeof(int)))){
       printf("Out of memory.\n");
       exit(40);
    }

    *p = * (int *) q;

     if ( *p != 22 ) exit(41);

     *p = 34;

      q = p;

      return (*fp)(q);

  }
  
  int use_int(void *r) {

    int p;

    p = *(int *)r;

   if ( p != 34 ) exit(42);

    return p;
  }
