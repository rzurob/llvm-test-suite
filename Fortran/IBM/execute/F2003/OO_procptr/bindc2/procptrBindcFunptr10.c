
#include <stdio.h>
#include <stdlib.h>

struct dt {
   int(*a)(int);
   int b;
};

void csub(struct dt *dtype) {

   extern int cfun(int);

   if ( dtype->b != 34 ) exit(41);

   dtype->a = &cfun;

}

int  cfun(int p) {

   if ( p != 22 ) exit(42); 

   return p;
}

