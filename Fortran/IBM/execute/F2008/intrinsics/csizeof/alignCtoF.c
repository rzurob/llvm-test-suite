#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <complex.h>


struct dt1 {
   _Bool d[2];
   long long int e;
};

struct dt2 {
   int a[10];
   short b[10];
};

struct dt3 {
   short    a;
   double   b; 
   char     c; 
   int      d; 
   long double _Complex  e; 
   long double   f; 
   _Bool   g; 
};

extern size_t getsize_dt1(struct dt1 data[2]);
extern size_t getsize_dt2(struct dt2 data[2][2]);
extern size_t getsize_dt3(struct dt3 data);

int main () {

    struct dt1 data1[2];
    struct dt2 data2[2][2];
    struct dt3 data3;

    if ( sizeof(data1) != getsize_dt1(data1) )
      return(EXIT_FAILURE);

    if ( sizeof(data2) != getsize_dt2(data2) )
      return(EXIT_FAILURE);

    if ( sizeof(data3) != getsize_dt3(data3) )
      return(EXIT_FAILURE);

    return 0;

}

