#include <stdio.h>
#include <stdlib.h>


struct dt {
   int a[5];
};

void initdt(struct dt *);

int main() {

   int fnt1(struct dt *);

   struct dt dta;
   int i, ret;

   ret = fnt1(&dta);

   for ( i = 0; i < 5; i++ ) if ( dta.a[i] != i+2 ) exit(41);
 
   if ( ret != 0 ) exit(42);

   return 0;
}


void initdt(struct dt *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
     if ( x->a[i] != 2 ) exit(43);
     x->a[i] = i+2; 
   }

}
