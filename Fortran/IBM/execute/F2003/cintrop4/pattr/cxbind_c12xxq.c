
/* The C code for test case "fxbind_c12xxq.f"  */
#include <stdio.h>
#include <assert.h>

void sub(int (*g)(int *)) {
   int ret, v;
   v = 2;
   printf("C = %d\n",g(&v));
   ret = g(&v);
   assert ( ret == 4 ) ;
}

int f(int *x) {
   return((*x)*(*x));
}

