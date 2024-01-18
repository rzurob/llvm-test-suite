#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <complex.h>


extern long array1[10];
extern long double array2[100];
extern long double _Complex  array3[20];

extern size_t array1_size();
extern size_t array2_size();
extern size_t array3_size();


int main () {

    if ( sizeof(array1) != array1_size() )
      return(EXIT_FAILURE);

    if ( sizeof(array2) != array2_size() )
      return(EXIT_FAILURE+1);

    if ( sizeof(array3) != array3_size() )
      return(EXIT_FAILURE+2);

    return 0;

}

