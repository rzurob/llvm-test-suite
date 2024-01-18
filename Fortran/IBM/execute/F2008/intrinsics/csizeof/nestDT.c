#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <complex.h>


struct dt1 {
   float a;
   int   b;
};

struct dt2 {
   struct dt1 dt[2];
   long double _Complex c;
};

extern size_t getsize_dt(struct dt2 data);

int main () {

    struct dt2 data;

    if ( sizeof(data) != getsize_dt(data) )
      return(EXIT_FAILURE);

    return 0;

}

