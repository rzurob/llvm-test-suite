#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "cmplx.h"

  int testf(float x, float y)
{   
   if (fabs(x - y) > 0.0001f) return 1; 
     else return 0 ;
}

 void sub_arr1(signed int (*a)[1][2][3], float (*b)[1][2][3]);

 void sub_arr2(char (*a)[1][2][3], char (*b)[1][2][3]);

 void sub_arr3( float _Complex (*a)[1][2][3]);
 
 
 void ssub_arr1(signed int (*a)[1][2][3], float (*b)[1][2][3]);

 void ssub_arr2(char (*a)[1][2][3], char (*b)[1][2][3]);

 void ssub_arr3( float _Complex (*a)[1][2][3]);
 
 int main() {
 
 
 signed int a1[1][2][3];
 signed int a2[1][2][3];
 signed int a3[1][2][3];
 
 float b1[1][2][3];
 float b2[1][2][3];
 float b3[1][2][3];
 
 char c[1][2][3];
 char l[1][2][3];
 
 float _Complex x1[1][2][3], x2[1][2][3], x3[1][2][3];
 
  int i,j,k;
  
   for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
     for(k=0;k<=2;k++){
         
         c[i][j][k] = 'a';
         l[i][j][k] = 0;
         
         a1[i][j][k] = 0;
         a2[i][j][k] = 3;
         a3[i][j][k] = 33;
         
         b1[i][j][k] = 0.0f;
         b2[i][j][k] = 3.4f;
         b3[i][j][k] = 33.4f;
         
         x1[i][j][k] = createcomplexf(0.0f, 0.0f);
         x2[i][j][k] = createcomplexf(1.0f, 3.0f);
         x3[i][j][k] = createcomplexf(10.0f, 30.0f);  
                       } 


   sub_arr1(&a1, &b1);
   sub_arr2(&c, &l);
   sub_arr3(&x1);
   
   for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
     for(k=0;k<=2;k++){
         if ( a1[i][j][k] != a2[i][j][k] ) exit(10);
         if ( testf(b1[i][j][k], b2[i][j][k]) ) exit(11);
         
         if ( c[i][j][k] != 'd' ) exit(20);
         if ( l[i][j][k] == 0 ) exit(21);
         
         if (testf(crealf(x1[i][j][k]), crealf(x2[i][j][k])) ||
             testf(cimagf(x1[i][j][k]), cimagf(x2[i][j][k])) ) exit(30);
                       
                       }
                       
   ssub_arr1(&a1, &b1);
   ssub_arr2(&c, &l);
   ssub_arr3(&x1);
   
   for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
     for(k=0;k<=2;k++){
         
         if ( a1[i][j][k] != a3[i][j][k] ) exit(12);
         if ( testf(b1[i][j][k], b3[i][j][k]) ) exit(13);
         
         if ( c[i][j][k] != 'a' ) exit(22);
         if ( l[i][j][k] != 0 ) exit(23);
         
         if (testf(crealf(x1[i][j][k]), crealf(x3[i][j][k])) ||
             testf(cimagf(x1[i][j][k]), cimagf(x3[i][j][k])) ) exit(31);
                       
                       }
                       
   exit(0);
   
 }
   
