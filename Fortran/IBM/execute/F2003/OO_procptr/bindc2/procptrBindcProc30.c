#define DIM1 4 
#define DIM2 5 

#include <stdio.h>
#include <stdlib.h>

void csubarray ( void **p )
{
   int i;
   int *a;

   a = (int *) *p;

   for ( i = 0; i < DIM1; i++ ) {
      if ( *(a+i) != i+1 ) exit(41);
   }

   for ( i = 0; i < DIM1; i++ ) {
      *(a+i) = i+2;
   }

}

void csubarray2 ( void **p )
{
   int i, j;
   int *a;

   a = (int *) *p;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         if ( *((a+i*DIM2)+j) != i+j+2 ) exit(43);
      }
   }

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         *((a+i*DIM2)+j) = i+j+3;
      }
   }

}

