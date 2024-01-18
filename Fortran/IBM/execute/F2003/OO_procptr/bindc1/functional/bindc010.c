#include <complex.h>

void print1 ( double i1[3], char i2[][2][2], float _Complex i3[3] )
{
   printf("here are the elements of i1: %f, %f, %f\n", i1[0], i1[1], i1[2] );
   printf("here are the elements of i2[0][0][0-1]: %c, %c\n", i2[0][0][0], i2[0][0][1] );
   printf("here are the elements of i2[0][1][0-1]: %c, %c\n", i2[0][1][0], i2[0][1][1] );
   printf("here are the elements of i2[1][0][0-1]: %c, %c\n", i2[1][0][0], i2[1][0][1] );
   printf("here are the elements of i2[1][1][0-1]: %c, %c\n", i2[1][1][0], i2[1][1][1] );
   printf("here are the elements of i3: (%f, %f), (%f, %f), (%f, %f)\n", creal(i3[0]), cimag(i3[0]),  creal(i3[1]), cimag(i3[1]), creal(i3[2]), cimag(i3[2]) );
}

_Bool print2 ( double i1[3], long double i2[][1] )
{
   printf("here are the elements of i1: %f, %f, %f\n", i1[0], i1[1], i1[2] );
   printf("here are the elements of i2[0][0]: %Lf\n", i2[0][0] );
   printf("here are the elements of i2[1][0]: %Lf\n", i2[1][0] );
   printf("here are the elements of i2[2][0]: %Lf\n", i2[2][0] );
   printf("here are the elements of i2[3][0]: %Lf\n", i2[3][0] );
   printf("here are the elements of i2[4][0]: %Lf\n", i2[4][0] );

   return ( i1[0] > 300 || i1[1] > 300 || i1[2] > 300 || i2[0][0] > 400 || i2[1][0] > 400 || i2[2][0] > 400 || i2[3][0] > 400 || i2[4][0] > 400);
}

