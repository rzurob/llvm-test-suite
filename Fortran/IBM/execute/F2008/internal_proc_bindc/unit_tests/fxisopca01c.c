
/*
        C code for testcase "fxisopca01.f"
*/

#include <stdio.h>
#include <stdlib.h>

#define DIM1 4
#define DIM2 3


void sub4(void (*funcp)()) {

   printf("Hello world sub4 in c\n");
   printf ("pointer value %x\n", funcp);
   (*funcp)();

}

