#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "cmplx.h"

  

 void extsub_arr1(signed int (*a)[1][2][3], float (*b)[1][2][3])
{
   int i,j,k;
  
   for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
     for(k=0;k<=2;k++){
         (*a)[i][j][k] = 3;
         (*b)[i][j][k] = 3.4;  
                       } 
  }


 void extsub_arr2(char (*a)[1][2][3], char (*b)[1][2][3])
{
   int i,j,k;
  
   for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
     for(k=0;k<=2;k++){
         (*a)[i][j][k] = 'a';
         (*b)[i][j][k] = 1;  
                       } 
  }


 void extsub_arr3( float _Complex (*a)[1][2][3])
{
   int i,j,k;
  
   for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
     for(k=0;k<=2;k++){
         (*a)[i][j][k] = createcomplexf(1.0f, 3.0f);
           
                       } 
  }
