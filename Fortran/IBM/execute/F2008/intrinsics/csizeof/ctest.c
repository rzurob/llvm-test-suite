#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <complex.h>

struct dttype1 {
   int a;
   short int b;
   long int c;
   long long int d;
};

struct dttype2 {
   int8_t a;
   int_least8_t b;
   int_fast8_t c;
   signed char d; 
};

struct dttype3 {
   int_fast16_t a;
   int_fast32_t b;
   int_fast64_t c;
};

struct dttype4 {
   int_least16_t a;
   int_least32_t b;
   int_least64_t c;
};

struct dttype5 {
   _Bool a;
   char b;
   intmax_t c;
};

struct dttype6 {
   int16_t a;
   int32_t b;
   int64_t c;
};

struct dttype7 {
   size_t a;
   intptr_t b;
};

struct dttypea {
   float a[8]; 
   double b[4];
   long double c;
};

struct dttypea1 {
   long double b;
};

struct dttypeb {
   float _Complex a[2];
   double _Complex b;
   long double _Complex c;
};

struct dttypec {
   int a[2];
   float b[2];  
};

struct {
   double a;
   int b;   
   short c;
   long double d;
} global1;

long myarray[10];

size_t get_global1_size( ) {
    return sizeof(global1);
}

size_t get_myarray_size( ) {
    return sizeof(myarray);
}

size_t get_size1( struct dttype1 dt ) {
    return sizeof(dt);
}

size_t get_size2( struct dttype2 dt ) {
    return sizeof(dt);
}

size_t get_size3( struct dttype3 dt ) {
    return sizeof(dt);
}

size_t get_size4( struct dttype4 dt ) {
    return sizeof(dt);
}

size_t get_size5( struct dttype5 dt ) {
    return sizeof(dt);
}

size_t get_size6( struct dttype6 dt ) {
    return sizeof(dt);
}

size_t get_size7( struct dttype7 dt ) {
    return sizeof(dt);
}

size_t get_sizea( struct dttypea dt ) {
    return sizeof(dt);
}

size_t get_sizea1( struct dttypea1 dt ) {
    return sizeof(dt);
}

size_t get_sizeb( struct dttypeb dt[10][10] ) {
    return 100*sizeof(dt[0][0]);
}

size_t get_sizec(struct dttypec dt, int num )
{
        printf(" %u %n ", sizeof(dt), num);
	return sizeof(dt)*num;
}

size_t get_ptrsize( int *p ) {
    return sizeof(p);
}

