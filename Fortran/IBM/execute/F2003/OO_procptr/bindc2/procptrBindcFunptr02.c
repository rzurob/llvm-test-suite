
#include <stdio.h>
#include <stdlib.h>


struct dt {
   int(*a)(int);
   int b;
};

void csub(int *t, struct dt *dtype ) {

   if ( dtype->b != 34 ) exit(41);

   *t = (*dtype->a)(dtype->b);

}

int  cfun(int p) {

   if ( p != 34 ) exit(42); 

   p = 22;

   return p;
}
