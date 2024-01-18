
#include <stdio.h>
#include <stdlib.h>

void bubble(int *work , int *size, int(**compare)(int , int)) {

   extern void swap(int *element1ptr, int *element2ptr);
   extern int ascending(int, int);

   int pass;
   int count; 

   for(pass = 1; pass <*size; pass++){
     for (count = 0; count<*size-1; count++) {
       if ((*compare)(work[count], work[count+1])){
         swap(&work[count], &work[count+1]);
       }
     }
   }
}

void swap(int *element1ptr, int *element2ptr)
{
    int hold;
    hold = *element1ptr;
    *element1ptr = *element2ptr;
    *element2ptr = hold;
}

int ascending(int a, int b){
     
    return b<a;
}

