
/*
        C code for testcase "fxisopxd02.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

#define DIM1 4
#define DIM2 3
#define DIM3 2

struct dts0 {
   long double _Complex a;
};

struct dts1 {
   long double _Complex b[DIM1];
};

struct dts2 {
   long double _Complex c[DIM2][DIM2];
};

void sub1(struct dts0 **p) {
   int i;

   for ( i = 0; i < DIM3; i++ ) {
      if ( (*p+i)->a != 5.0l+I*5.0l ) exit(21);
   }

   *p = malloc(sizeof(**p)*DIM3);

   for ( i = 0; i < DIM3; i++ ) {
      (*p+i)->a = 10.0l+I*10.0l;
   }
}

void sub2(struct dts1 **p) {
   int i, j;

   for ( i = 0; i < DIM3; i++ ) {
      for ( j = 0; j < DIM1; j++ ) {
         if ( (*p)->b[i*DIM1+j] != (long double)(j+1)+I*(long double)(j+1) ) exit(23);
      }
   }

   *p = malloc(sizeof(**p)*DIM1*DIM3);

   for ( i = 0; i < DIM3; i++ ) {
      for ( j = 0; j < DIM1; j++ ) {
         (*p)->b[i*DIM1+j] = (long double)(j+2)+I*(long double)(j+2);
      }
   }
}

void sub3(struct dts2 **p) {
   int i, j, k, l;

   for ( i = 0; i < DIM3; i++ )
      for ( j = 0; j < DIM3; j++ )
         for ( k = 0; k < DIM2; k++ )
            for ( l = 0; l < DIM2; l++ )
               if ( (*p+i*DIM3+j)->c[k][l] != (long double)(k+l+2)+I*(long double)(k+l+2) ) exit(25);

   *p = malloc(sizeof(**p)*DIM2*DIM2*DIM3*DIM3);

   for ( i = 0; i < DIM3; i++ )
      for ( j = 0; j < DIM3; j++ )
         for ( k = 0; k < DIM2; k++ )
            for ( l = 0; l < DIM2; l++ )
               (*p+i*DIM3+j)->c[k][l] = (long double)(k+l+3)+I*(long double)(k+l+3);
}