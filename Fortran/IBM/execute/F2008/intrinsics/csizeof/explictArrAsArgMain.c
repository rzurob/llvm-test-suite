#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>


extern long array1[10];
extern long double array2[10];
extern size_t array1_size();
extern size_t array2_size();

int main () {

    if ( sizeof(array1) != array1_size() )
      return(EXIT_FAILURE);

    if ( sizeof(array2) != array2_size() )
      return(EXIT_FAILURE+1);

    return 0;
}

