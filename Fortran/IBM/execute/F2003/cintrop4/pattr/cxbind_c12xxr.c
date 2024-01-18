
/* The C code for test case "fxbind_c12xxr.f"  */
#include <stdio.h>
#include <assert.h>

void sub(int (**p)(int *)) {
   int ret, v;
   v = 2;
   printf("C = %d\n",(*p)(&v));
   ret = (*p)(&v);
   assert ( ret == 4 ) ;
}

