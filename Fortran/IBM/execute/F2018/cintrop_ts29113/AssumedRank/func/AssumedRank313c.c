#include <stdio.h>
#include <complex.h>
#include <stdint.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

#define true 1

typedef struct dt0 DT0;
typedef struct dt1 DT1;

struct dt0 {
   char a;
   int b;
};

struct dt1 {
   char a;
   int b;
   struct dt0 d0;
};


void c_sub(CFI_cdesc_t * a, int flag)
{

      int rc;

      if (a == NULL)
      {
          fprintf(stderr, "Expecting the arg. to be present... \n");
          printf("...exiting.\n");
          exit(1);
      }

   if(flag == 1){
      assert(a->rank == 2);
      assert(a->type == CFI_type_short);
      assert(a->dim[0].extent == 2);
      assert(a->dim[1].extent == 2);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[0].sm == 2);
      assert(a->dim[1].sm == (a->dim[0].sm*a->dim[0].extent));

      short *address;
      CFI_index_t subscripts[2];
      subscripts[0] = 0;
      subscripts[1] = 0;
      address = (short *) CFI_address(a, subscripts);
      printf("Type is short, the received value is %hd\n", *address);

      fflush(stdout);
      }
   else if(flag == 2){
      assert(a->rank == 1);
      assert(a->type == CFI_type_int);
      assert(a->dim[0].extent == 8);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[0].sm == 4);

      int *address;
      CFI_index_t subscripts[1];
      subscripts[0] = 0;
      address = (int *) CFI_address(a, subscripts);
      printf("Type is int, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 3){
      assert(a->rank == 1);
#ifdef __64BIT__
      assert(a->type == CFI_type_long_long);
      assert(a->dim[0].sm == 8);
#else
      assert(a->type == CFI_type_int);
      assert(a->dim[0].sm == 4);
#endif 
      assert(a->dim[0].extent == 4);
      assert(a->dim[0].lower_bound == 0);

      long *address;
      CFI_index_t subscripts[1];
      subscripts[0] = 0;
      address = (long *) CFI_address(a, subscripts);
      printf("Type is long, the received value is %ld\n", *address);
      fflush(stdout);
      }
   else if(flag == 4){
      assert(a->rank == 1);
      assert(a->type == CFI_type_long_long);
      assert(a->dim[0].extent == 2);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[0].sm == 8);

      long long *address;
      CFI_index_t subscripts[1];
      subscripts[0] = 0;
      address = (long long *) CFI_address(a, subscripts);
      printf("Type is long long, the received value is %lld\n", *address);
      fflush(stdout);
      }
   else if(flag == 5){
      assert(a->rank == 4);
      assert(a->type == CFI_type_signed_char);
      assert(a->dim[0].extent == 1);
      assert(a->dim[1].extent == 2);
      assert(a->dim[2].extent == 3);
      assert(a->dim[3].extent == 4);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[2].lower_bound == 0);
      assert(a->dim[3].lower_bound == 0);
      assert(a->dim[0].sm == 1);
      assert(a->dim[1].sm == 1);
      assert(a->dim[2].sm == 2);
      assert(a->dim[3].sm == 6);

      signed char *address;
      CFI_index_t subscripts[4];
      subscripts[0] = 0;
      subscripts[1] = 0;
      subscripts[2] = 0;
      subscripts[3] = 0;
      address = (signed char *) CFI_address(a, subscripts);
      printf("Type is signed char, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 6){
      assert(a->rank == 2);
      printf("test  %d\n", a->type);
      assert(a->type == CFI_type_signed_char);
      assert(a->dim[0].extent == 21);
      assert(a->dim[1].extent == 11);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[0].sm ==  1);
      assert(a->dim[1].sm == 21);

      int8_t *address;
      CFI_index_t subscripts[2];
      subscripts[0] = 0;
      subscripts[1] = 0;
      address = (int8_t *) CFI_address(a, subscripts);
      printf("Type is int8_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 7){
      assert(a->rank == 2);
      assert(a->type == CFI_type_short);
      assert(a->dim[0].extent == 2);
      assert(a->dim[1].extent == 1);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[0].sm == 2);
      assert(a->dim[1].sm == 2*a->dim[0].sm);

      int16_t *address;
      CFI_index_t subscripts[2];
      subscripts[0] = 0;
      subscripts[1] = 0;
      address = (int16_t *) CFI_address(a, subscripts);
      printf("Type is int16_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 8){
      assert(a->rank == 1);
      assert(a->type == CFI_type_int32_t);
      assert(a->dim[0].extent == 1);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[0].sm == 4);

      int32_t *address;
      CFI_index_t subscripts[1];
      subscripts[0] = 0;
      address = (int32_t *) CFI_address(a, subscripts);
      printf("Type is int32_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 9){
      assert(a->rank == 1);
      assert(a->type == CFI_type_int64_t);
      assert(a->dim[0].extent == 100);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[0].sm == 8);

      int64_t *address;
      CFI_index_t subscripts[1];
      subscripts[0] = 0;
      address = (int64_t *) CFI_address(a, subscripts);
      printf("Type is int64_t, the received value is %lld\n", *address);
      fflush(stdout);
      }
   else if(flag == 10){
      assert(a->rank == 1);
      assert(a->dim[0].extent == 1);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[0].sm == 1);

      int_least8_t *address;
      CFI_index_t subscripts[1];
      subscripts[0] = 0;
      address = (int_least8_t *) CFI_address(a, subscripts);
      printf("Type is int_least8_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 11){
      assert(a->rank == 2);
      assert(a->dim[0].extent == 1);
      assert(a->dim[1].extent == 1);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[0].sm == 2);
      assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);

      int_least16_t *address;
      CFI_index_t subscripts[2];
      subscripts[0] = 0;
      subscripts[1] = 0;
      address = (int_least16_t *) CFI_address(a, subscripts);
      printf("Type is int_least16_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 12){
      assert(a->rank == 3);
      assert(a->dim[0].extent == 1);
      assert(a->dim[1].extent == 1);
      assert(a->dim[2].extent == 1);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[2].lower_bound == 0);
      assert(a->dim[0].sm == 4);
      assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);
      assert(a->dim[2].sm == a->dim[1].extent*a->dim[1].sm);

      int_least32_t *address;
      CFI_index_t subscripts[3];
      subscripts[0] = 0;
      subscripts[1] = 0;
      subscripts[2] = 0;
      address = (int_least32_t *) CFI_address(a, subscripts);
      printf("Type is int_least32_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 13){
      assert(a->rank == 4);
      assert(a->dim[0].extent == 1);
      assert(a->dim[1].extent == 1);
      assert(a->dim[2].extent == 1);
      assert(a->dim[3].extent == 1);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[2].lower_bound == 0);
      assert(a->dim[3].lower_bound == 0);
      assert(a->dim[0].sm == 8);
      assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);
      assert(a->dim[2].sm == a->dim[1].extent*a->dim[1].sm);
      assert(a->dim[3].sm == a->dim[2].extent*a->dim[2].sm);

      int_least64_t *address;
      CFI_index_t subscripts[4];
      subscripts[0] = 0;
      subscripts[1] = 0;
      subscripts[2] = 0;
      subscripts[3] = 0;
      address = (int_least64_t *) CFI_address(a, subscripts);
      printf("Type is int_least64_t, the received value is %lld\n", *address);
      fflush(stdout);
      }
   else if(flag == 14){
      assert(a->rank == 5);
      assert(a->dim[0].extent == 2);
      assert(a->dim[1].extent == 2);
      assert(a->dim[2].extent == 2);
      assert(a->dim[3].extent == 2);
      assert(a->dim[4].extent == 2);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[2].lower_bound == 0);
      assert(a->dim[3].lower_bound == 0);
      assert(a->dim[4].lower_bound == 0);
      assert(a->dim[0].sm == 1);
      assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);
      assert(a->dim[2].sm == a->dim[1].extent*a->dim[1].sm);
      assert(a->dim[3].sm == a->dim[2].extent*a->dim[2].sm);
      assert(a->dim[4].sm == a->dim[3].extent*a->dim[3].sm);

      int_fast8_t *address;
      CFI_index_t subscripts[5];
      subscripts[0] = 0;
      subscripts[1] = 0;
      subscripts[2] = 0;
      subscripts[3] = 0;
      subscripts[4] = 0;
      address = (int_fast8_t *) CFI_address(a, subscripts);
      printf("Type is int_fast8_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 15){
      assert(a->rank == 6);
      assert(a->dim[0].extent == 2);
      assert(a->dim[1].extent == 2);
      assert(a->dim[2].extent == 2);
      assert(a->dim[3].extent == 2);
      assert(a->dim[4].extent == 2);
      assert(a->dim[5].extent == 2);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[2].lower_bound == 0);
      assert(a->dim[3].lower_bound == 0);
      assert(a->dim[4].lower_bound == 0);
      assert(a->dim[5].lower_bound == 0);
#ifdef __linux__
    #ifdef __64BIT__
      assert(a->dim[0].sm == 8);
    #else
      assert(a->dim[0].sm == 4);
    #endif
#else
      assert(a->dim[0].sm == 2);
#endif
      assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);
      assert(a->dim[2].sm == a->dim[1].extent*a->dim[1].sm);
      assert(a->dim[3].sm == a->dim[2].extent*a->dim[2].sm);
      assert(a->dim[4].sm == a->dim[3].extent*a->dim[3].sm);
      assert(a->dim[5].sm == a->dim[4].extent*a->dim[4].sm);

      int_fast16_t *address;
      CFI_index_t subscripts[6];
      subscripts[0] = 0;
      subscripts[1] = 0;
      subscripts[2] = 0;
      subscripts[3] = 0;
      subscripts[4] = 0;
      subscripts[5] = 0;
      address = (int_fast16_t *) CFI_address(a, subscripts);
      printf("Type is int_fast16_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 16){
      assert(a->rank == 7);
      assert(a->dim[0].extent == 2);
      assert(a->dim[1].extent == 2);
      assert(a->dim[2].extent == 2);
      assert(a->dim[3].extent == 2);
      assert(a->dim[4].extent == 2);
      assert(a->dim[5].extent == 2);
      assert(a->dim[6].extent == 2);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[2].lower_bound == 0);
      assert(a->dim[3].lower_bound == 0);
      assert(a->dim[4].lower_bound == 0);
      assert(a->dim[5].lower_bound == 0);
      assert(a->dim[6].lower_bound == 0);
#ifdef __linux__
    #ifdef __64BIT__
      assert(a->dim[0].sm == 8);
    #else
      assert(a->dim[0].sm == 4);
    #endif
#else
      assert(a->dim[0].sm == 4);
#endif
      assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);
      assert(a->dim[2].sm == a->dim[1].extent*a->dim[1].sm);
      assert(a->dim[3].sm == a->dim[2].extent*a->dim[2].sm);
      assert(a->dim[4].sm == a->dim[3].extent*a->dim[3].sm);
      assert(a->dim[5].sm == a->dim[4].extent*a->dim[4].sm);
      assert(a->dim[6].sm == a->dim[5].extent*a->dim[5].sm);

      int_fast32_t *address;
      CFI_index_t subscripts[7];
      subscripts[0] = 0;
      subscripts[1] = 0;
      subscripts[2] = 0;
      subscripts[3] = 0;
      subscripts[4] = 0;
      subscripts[5] = 0;
      subscripts[6] = 0;
      address = (int_fast32_t *) CFI_address(a, subscripts);
      printf("Type is int_fast32_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 17){
      assert(a->rank == 8);
      assert(a->dim[0].extent == 2);
      assert(a->dim[1].extent == 2);
      assert(a->dim[2].extent == 2);
      assert(a->dim[3].extent == 2);
      assert(a->dim[4].extent == 2);
      assert(a->dim[5].extent == 2);
      assert(a->dim[6].extent == 2);
      assert(a->dim[7].extent == 2);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[2].lower_bound == 0);
      assert(a->dim[3].lower_bound == 0);
      assert(a->dim[4].lower_bound == 0);
      assert(a->dim[5].lower_bound == 0);
      assert(a->dim[6].lower_bound == 0);
      assert(a->dim[7].lower_bound == 0);
      assert(a->dim[0].sm == 8);
      assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);
      assert(a->dim[2].sm == a->dim[1].extent*a->dim[1].sm);
      assert(a->dim[3].sm == a->dim[2].extent*a->dim[2].sm);
      assert(a->dim[4].sm == a->dim[3].extent*a->dim[3].sm);
      assert(a->dim[5].sm == a->dim[4].extent*a->dim[4].sm);
      assert(a->dim[6].sm == a->dim[5].extent*a->dim[5].sm);
      assert(a->dim[7].sm == a->dim[6].extent*a->dim[6].sm);

      int_fast64_t *address;
      CFI_index_t subscripts[8];
      subscripts[0] = 0;
      subscripts[1] = 0;
      subscripts[2] = 0;
      subscripts[3] = 0;
      subscripts[4] = 0;
      subscripts[5] = 0;
      subscripts[6] = 0;
      subscripts[7] = 0;
      address = (int_fast64_t *) CFI_address(a, subscripts);
      printf("Type is int_fast64_t, the received value is %lld\n", *address);
      fflush(stdout);
      }
   else if(flag == 18){
      assert(a->rank == 15);
      assert(a->dim[0].extent == 1);
      assert(a->dim[1].extent == 1);
      assert(a->dim[2].extent == 1);
      assert(a->dim[3].extent == 1);
      assert(a->dim[4].extent == 1);
      assert(a->dim[5].extent == 1);
      assert(a->dim[6].extent == 1);
      assert(a->dim[7].extent == 1);
      assert(a->dim[8].extent == 1);
      assert(a->dim[9].extent == 1);
      assert(a->dim[10].extent == 1);
      assert(a->dim[11].extent == 1);
      assert(a->dim[12].extent == 1);
      assert(a->dim[13].extent == 1);
      assert(a->dim[14].extent == 1);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[2].lower_bound == 0);
      assert(a->dim[3].lower_bound == 0);
      assert(a->dim[4].lower_bound == 0);
      assert(a->dim[5].lower_bound == 0);
      assert(a->dim[6].lower_bound == 0);
      assert(a->dim[7].lower_bound == 0);
      assert(a->dim[8].lower_bound == 0);
      assert(a->dim[9].lower_bound == 0);
      assert(a->dim[10].lower_bound == 0);
      assert(a->dim[11].lower_bound == 0);
      assert(a->dim[12].lower_bound == 0);
      assert(a->dim[13].lower_bound == 0);
      assert(a->dim[14].lower_bound == 0);
      assert(a->dim[0].sm == 8);
      assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);
      assert(a->dim[2].sm == a->dim[1].extent*a->dim[1].sm);
      assert(a->dim[3].sm == a->dim[2].extent*a->dim[2].sm);
      assert(a->dim[4].sm == a->dim[3].extent*a->dim[3].sm);
      assert(a->dim[5].sm == a->dim[4].extent*a->dim[4].sm);
      assert(a->dim[6].sm == a->dim[5].extent*a->dim[5].sm);
      assert(a->dim[7].sm == a->dim[6].extent*a->dim[6].sm);
      assert(a->dim[8].sm == a->dim[7].extent*a->dim[7].sm);
      assert(a->dim[9].sm == a->dim[8].extent*a->dim[8].sm);
      assert(a->dim[10].sm == a->dim[9].extent*a->dim[9].sm);
      assert(a->dim[11].sm == a->dim[10].extent*a->dim[10].sm);
      assert(a->dim[12].sm == a->dim[11].extent*a->dim[11].sm);
      assert(a->dim[13].sm == a->dim[12].extent*a->dim[12].sm);
      assert(a->dim[14].sm == a->dim[13].extent*a->dim[13].sm);

      intmax_t *address;
      CFI_index_t subscripts[15];
      subscripts[0] = 0;
      subscripts[1] = 0;
      subscripts[2] = 0;
      subscripts[3] = 0;
      subscripts[4] = 0;
      subscripts[5] = 0;
      subscripts[6] = 0;
      subscripts[7] = 0;
      subscripts[8] = 0;
      subscripts[9] = 0;
      subscripts[10] = 0;
      subscripts[11] = 0;
      subscripts[12] = 0;
      subscripts[13] = 0;
      subscripts[14] = 0;
      address = (intmax_t *) CFI_address(a, subscripts);
      printf("Type is intmax_t, the received value is %lld\n", *address);
      fflush(stdout);
      }
   else if(flag == 19){
      assert(a->rank == 15);
#ifdef __64BIT__
      assert(a->type == CFI_type_long_long);
      assert(a->dim[0].sm == 8);
#else
      assert(a->type == CFI_type_int);
      assert(a->dim[0].sm == 4);
#endif
      assert(a->dim[0].extent == 1);
      assert(a->dim[1].extent == 1);
      assert(a->dim[2].extent == 1);
      assert(a->dim[3].extent == 1);
      assert(a->dim[4].extent == 1);
      assert(a->dim[5].extent == 1);
      assert(a->dim[6].extent == 1);
      assert(a->dim[7].extent == 1);
      assert(a->dim[8].extent == 1);
      assert(a->dim[9].extent == 1);
      assert(a->dim[10].extent == 1);
      assert(a->dim[11].extent == 1);
      assert(a->dim[12].extent == 1);
      assert(a->dim[13].extent == 1);
      assert(a->dim[14].extent == 1);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[2].lower_bound == 0);
      assert(a->dim[3].lower_bound == 0);
      assert(a->dim[4].lower_bound == 0);
      assert(a->dim[5].lower_bound == 0);
      assert(a->dim[6].lower_bound == 0);
      assert(a->dim[7].lower_bound == 0);
      assert(a->dim[8].lower_bound == 0);
      assert(a->dim[9].lower_bound == 0);
      assert(a->dim[10].lower_bound == 0);
      assert(a->dim[11].lower_bound == 0);
      assert(a->dim[12].lower_bound == 0);
      assert(a->dim[13].lower_bound == 0);
      assert(a->dim[14].lower_bound == 0);
      assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);
      assert(a->dim[2].sm == a->dim[1].extent*a->dim[1].sm);
      assert(a->dim[3].sm == a->dim[2].extent*a->dim[2].sm);
      assert(a->dim[4].sm == a->dim[3].extent*a->dim[3].sm);
      assert(a->dim[5].sm == a->dim[4].extent*a->dim[4].sm);
      assert(a->dim[6].sm == a->dim[5].extent*a->dim[5].sm);
      assert(a->dim[7].sm == a->dim[6].extent*a->dim[6].sm);
      assert(a->dim[8].sm == a->dim[7].extent*a->dim[7].sm);
      assert(a->dim[9].sm == a->dim[8].extent*a->dim[8].sm);
      assert(a->dim[10].sm == a->dim[9].extent*a->dim[9].sm);
      assert(a->dim[11].sm == a->dim[10].extent*a->dim[10].sm);
      assert(a->dim[12].sm == a->dim[11].extent*a->dim[11].sm);
      assert(a->dim[13].sm == a->dim[12].extent*a->dim[12].sm);
      assert(a->dim[14].sm == a->dim[13].extent*a->dim[13].sm);

      intptr_t *address;
      CFI_index_t subscripts[15];
      subscripts[0] = 0;
      subscripts[1] = 0;
      subscripts[2] = 0;
      subscripts[3] = 0;
      subscripts[4] = 0;
      subscripts[5] = 0;
      subscripts[6] = 0;
      subscripts[7] = 0;
      subscripts[8] = 0;
      subscripts[9] = 0;
      subscripts[10] = 0;
      subscripts[11] = 0;
      subscripts[12] = 0;
      subscripts[13] = 0;
      subscripts[14] = 0;
      address = (intptr_t *) CFI_address(a, subscripts);
      printf("Type is intptr_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 20){
      assert(a->rank == 3); 
      assert(a->type == CFI_type_float);
      assert(a->dim[0].extent == 5);
      assert(a->dim[1].extent == 1);
      assert(a->dim[2].extent == 3);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[2].lower_bound == 0);
      assert(a->dim[0].sm == 4);
      assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);
      assert(a->dim[2].sm == a->dim[1].extent*a->dim[1].sm);

      float *address;
      CFI_index_t subscripts[3];
      subscripts[0] = 0;
      subscripts[1] = 0;
      subscripts[2] = 0;
      address = (float *) CFI_address(a, subscripts);
      printf("Type is float, the received value is %f\n", *address);
      fflush(stdout);
      }
   else if(flag == 21){
      assert(a->rank == 1);
      assert(a->type == CFI_type_double);
      assert(a->dim[0].extent == 22);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[0].sm == 8);

      double *address;
      CFI_index_t subscripts[1];
      subscripts[0] = 0;
      address = (double *) CFI_address(a, subscripts);
      printf("Type is double, the received value is %f\n", *address);
      fflush(stdout);
      }
   else if(flag == 22){
      assert(a->rank == 1);
      assert(a->type == CFI_type_long_double);
      assert(a->dim[0].extent == 100);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[0].sm == 16);


      long double *address;
      CFI_index_t subscripts[1];
      subscripts[0] = 0;
      address = (long double *) CFI_address(a, subscripts);
      printf("Type is long double, the received value is %Lf\n", *address);
      fflush(stdout);
      }
   else if(flag == 23){
      assert(a->rank == 2);
      assert(a->type == CFI_type_float_Complex);
      assert(a->dim[0].extent == 5);
      assert(a->dim[1].extent == 5);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[0].sm == 8);
      assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);

      float complex *address;
      CFI_index_t subscripts[2];
      subscripts[0] = 0;
      subscripts[1] = 0;
      address = (float complex *) CFI_address(a, subscripts);
      printf("Type is float complex, the received value is (%f, %f)\n", crealf(*address), cimagf(*address));
      fflush(stdout);
      }
   else if(flag == 24){
      assert(a->rank == 2);
      assert(a->type == CFI_type_double_Complex);
      assert(a->dim[0].extent == 5);
      assert(a->dim[1].extent == 5);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[0].sm == 16);
      assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);

      double complex *address;
      CFI_index_t subscripts[2];
      subscripts[0] = 0;
      subscripts[1] = 0;
      address = (double complex *) CFI_address(a, subscripts);
      printf("Type is double complex, the received value is (%f, %f)\n", crealf(*address), cimagf(*address));
      fflush(stdout);
      }
   else if(flag == 25){
      assert(a->rank == 1);
      assert(a->type == CFI_type_long_double_Complex);
      assert(a->dim[0].extent == 1);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[0].sm == 32);

      long double complex *address;
      CFI_index_t subscripts[1];
      subscripts[0] = 0;
      address = (long double complex *) CFI_address(a, subscripts);
#ifdef __linux__
      printf("Type is long double complex, the received value is (%llf, %llf)\n", creall(*address), cimagl(*address));
#else
    // functions creall and cimagl not implemented on AIX for long double 128. Use these for now
    // See Design change request (DCR) number MR1027116315
      printf("Type is long double complex, the received value is (%Lf, %Lf)\n", crealf(*address), cimagf(*address));
#endif
      fflush(stdout);
      }
   else if(flag == 26){
      assert(a->rank == 14);
      assert(a->type == CFI_type_Bool);
      assert(a->dim[0].extent == 1);
      assert(a->dim[1].extent == 1);
      assert(a->dim[2].extent == 1);
      assert(a->dim[3].extent == 1);
      assert(a->dim[4].extent == 1);
      assert(a->dim[5].extent == 1);
      assert(a->dim[6].extent == 1);
      assert(a->dim[7].extent == 1);
      assert(a->dim[8].extent == 1);
      assert(a->dim[9].extent == 1);
      assert(a->dim[10].extent == 1);
      assert(a->dim[11].extent == 1);
      assert(a->dim[12].extent == 1);
      assert(a->dim[13].extent == 10);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[2].lower_bound == 0);
      assert(a->dim[3].lower_bound == 0);
      assert(a->dim[4].lower_bound == 0);
      assert(a->dim[5].lower_bound == 0);
      assert(a->dim[6].lower_bound == 0);
      assert(a->dim[7].lower_bound == 0);
      assert(a->dim[8].lower_bound == 0);
      assert(a->dim[9].lower_bound == 0);
      assert(a->dim[10].lower_bound == 0);
      assert(a->dim[11].lower_bound == 0);
      assert(a->dim[12].lower_bound == 0);
      assert(a->dim[13].lower_bound == 0);
      assert(a->dim[0].sm == 1);
      assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);
      assert(a->dim[2].sm == a->dim[1].extent*a->dim[1].sm);
      assert(a->dim[3].sm == a->dim[2].extent*a->dim[2].sm);
      assert(a->dim[4].sm == a->dim[3].extent*a->dim[3].sm);
      assert(a->dim[5].sm == a->dim[4].extent*a->dim[4].sm);
      assert(a->dim[6].sm == a->dim[5].extent*a->dim[5].sm);
      assert(a->dim[7].sm == a->dim[6].extent*a->dim[6].sm);
      assert(a->dim[8].sm == a->dim[7].extent*a->dim[7].sm);
      assert(a->dim[9].sm == a->dim[8].extent*a->dim[8].sm);
      assert(a->dim[10].sm == a->dim[9].extent*a->dim[9].sm);
      assert(a->dim[11].sm == a->dim[10].extent*a->dim[10].sm);
      assert(a->dim[12].sm == a->dim[11].extent*a->dim[11].sm);
      assert(a->dim[13].sm == a->dim[12].extent*a->dim[12].sm);

      _Bool *address;
      CFI_index_t subscripts[14];
      subscripts[0] = 0;
      subscripts[1] = 0;
      subscripts[2] = 0;
      subscripts[3] = 0;
      subscripts[4] = 0;
      subscripts[5] = 0;
      subscripts[6] = 0;
      subscripts[7] = 0;
      subscripts[8] = 0;
      subscripts[9] = 0;
      subscripts[10] = 0;
      subscripts[11] = 0;
      subscripts[12] = 0;
      subscripts[13] = 0;
      address = (_Bool *) CFI_address(a, subscripts);
      printf("Type is _Bool, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 27){
      assert(a->rank == 1);
      assert(a->type == CFI_type_char);
      assert(a->dim[0].extent == 1024);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[0].sm == 1);

      char *address;
      CFI_index_t subscripts[1];
      subscripts[0] = 0;
      address = (char *) CFI_address(a, subscripts);
      printf("Type is char, the received value is %c\n", *address);
      fflush(stdout);
      }
   else if(flag == 28){
      assert(a->rank == 5);
      assert(a->type == CFI_type_struct);
      assert(a->dim[0].extent == 1);
      assert(a->dim[1].extent == 2);
      assert(a->dim[2].extent == 3);
      assert(a->dim[3].extent == 4);
      assert(a->dim[4].extent == 5);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[1].lower_bound == 0);
      assert(a->dim[2].lower_bound == 0);
      assert(a->dim[3].lower_bound == 0);
      assert(a->dim[4].lower_bound == 0);
      assert(a->dim[0].sm == 8); // value depends on the padding. Should not be tested?
      assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);
      assert(a->dim[2].sm == a->dim[1].extent*a->dim[1].sm);
      assert(a->dim[3].sm == a->dim[2].extent*a->dim[2].sm);
      assert(a->dim[4].sm == a->dim[3].extent*a->dim[3].sm);

      DT0 *address;
      CFI_index_t subscripts[5];
      subscripts[0] = 0;
      subscripts[1] = 0;
      subscripts[2] = 0;
      subscripts[3] = 0;
      subscripts[4] = 0;
      address = (DT0 *) CFI_address(a, subscripts);
      printf("Type is struct, the received value is %c %d\n", address->a, address->b);
      fflush(stdout);
      }
   else if(flag == 29){
      assert(a->rank == 1);
      assert(a->type == CFI_type_struct);
      assert(a->dim[0].extent == 11);
      assert(a->dim[0].lower_bound == 0);
      assert(a->dim[0].sm == 16); // value depends on the padding. Should not be tested?

      DT1 *address;
      CFI_index_t subscripts[1];
      subscripts[0] = 0;
      address = (DT1 *) CFI_address(a, subscripts);
      printf("Type is struct, the received value is %c %d %c %d\n", address->a, address->b, address->d0.a, address->d0.b);
      fflush(stdout);
      }

  return;
}
