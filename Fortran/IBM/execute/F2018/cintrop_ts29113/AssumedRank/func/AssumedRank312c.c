#include <stdio.h>
#include <complex.h>
#include <stdint.h>
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

      if (a->base_addr == NULL)
      {
          fprintf(stderr, "Expecting the arg. to be present... \n");
          printf("...exiting.\n");
          exit(1);
      }

      if (a->rank != 0)
      {
          fprintf(stderr, "Expecting a scalar...\n");
          printf("...exiting.\n");
          exit(2);
      }

   if(flag == 1){
      if (a->type != CFI_type_short)
      {
          fprintf(stderr, "Expecting object of type short...\n");
          printf("...exiting.\n");
          exit(3);
      }
      short *address;
      address = (short *) CFI_address(a, NULL);
      printf("Type is short, the received value is %hd\n", *address);
      fflush(stdout);
      }
   else if(flag == 2){
      if (a->type != CFI_type_int)
      {
          fprintf(stderr, "Expecting object of type int...\n");
          printf("...exiting.\n");
          exit(4);
      }
      int *address;
      address = (int *) CFI_address(a, NULL);
      printf("Type is int, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 3){

#ifdef __64BIT__
      if (a->type != CFI_type_long_long)
      {
          fprintf(stderr, "Expecting object of type long...\n");
          printf("...exiting.\n");
          exit(5);
      }
#else
      if (a->type != CFI_type_int)
      {
          fprintf(stderr, "Expecting object of type long...\n");
          printf("...exiting.\n");
          exit(5);
      }
#endif

      long *address;
      address = (long *) CFI_address(a, NULL);
      printf("Type is long, the received value is %ld\n", *address);
      fflush(stdout);
      }
   else if(flag == 4){
      if (a->type != CFI_type_long_long)
      {
          fprintf(stderr, "Expecting object of type long long...\n");
          printf("...exiting.\n");
          exit(6);
      }
      long long *address;
      address = (long long *) CFI_address(a, NULL);
      printf("Type is long long, the received value is %lld\n", *address);
      fflush(stdout);
      }
   else if(flag == 5){
      if (a->type != CFI_type_signed_char)
      {
          fprintf(stderr, "Expecting object of type signed char...\n");
          printf("...exiting.\n");
          exit(7);
      }
      signed char *address;
      address = (signed char *) CFI_address(a, NULL);
      printf("Type is signed char, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 6){
      if (a->type != CFI_type_signed_char)
      {
          fprintf(stderr, "Expecting object of type int8_t...\n");
          printf("...exiting.\n");
          exit(8);
      }
      int8_t *address;
      address = (int8_t *) CFI_address(a, NULL);
      printf("Type is int8_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 7){
      if (a->type != CFI_type_int16_t)
      {
          fprintf(stderr, "Expecting object of type int16_t...\n");
          printf("...exiting.\n");
          exit(9);
      }
      int16_t *address;
      address = (int16_t *) CFI_address(a, NULL);
      printf("Type is int16_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 8){
      if (a->type != CFI_type_int32_t)
      {
          fprintf(stderr, "Expecting object of type int32_t...\n");
          printf("...exiting.\n");
          exit(10);
      }
      int32_t *address;
      address = (int32_t *) CFI_address(a, NULL);
      printf("Type is int32_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 9){
      if (a->type != CFI_type_int64_t)
      {
          fprintf(stderr, "Expecting object of type int64_t...\n");
          printf("...exiting.\n");
          exit(11);
      }
      int64_t *address;
      address = (int64_t *) CFI_address(a, NULL);
      printf("Type is int64_t, the received value is %lld\n", *address);
      fflush(stdout);
      }
   else if(flag == 10){
      if (a->type != CFI_type_signed_char)
      {
          fprintf(stderr, "Expecting object of type int_least8_t...\n");
          printf("...exiting.\n");
          exit(12);
      }
      int_least8_t *address;
      address = (int_least8_t *) CFI_address(a, NULL);
      printf("Type is int_least8_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 11){
      if (a->type != CFI_type_int_least16_t)
      {
          fprintf(stderr, "Expecting object of type int_least16_t...\n");
          printf("...exiting.\n");
          exit(13);
      }
      int_least16_t *address;
      address = (int_least16_t *) CFI_address(a, NULL);
      printf("Type is int_least16_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 12){
      if (a->type != CFI_type_int_least32_t)
      {
          fprintf(stderr, "Expecting object of type int_least32_t...\n");
          printf("...exiting.\n");
          exit(14);
      }
      int_least32_t *address;
      address = (int_least32_t *) CFI_address(a, NULL);
      printf("Type is int_least32_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 13){
      if (a->type != CFI_type_int_least64_t)
      {
          fprintf(stderr, "Expecting object of type int_least64_t...\n");
          printf("...exiting.\n");
          exit(15);
      }
      int_least64_t *address;
      address = (int_least64_t *) CFI_address(a, NULL);
      printf("Type is int_least64_t, the received value is %lld\n", *address);
      fflush(stdout);
      }
   else if(flag == 14){
      if (a->type != CFI_type_signed_char)
      {
          fprintf(stderr, "Expecting object of type int_fast8_t...\n");
          printf("...exiting.\n");
          exit(16);
      }
      int_fast8_t *address;
      address = (int_fast8_t *) CFI_address(a, NULL);
      printf("Type is int_fast8_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 15){
#ifdef __linux__
    #ifdef __64BIT__
      if (a->type != CFI_type_long_long)
      {
          fprintf(stderr, "Expecting object of int_fast16_t...\n");
          printf("...exiting.\n");
          exit(17);
      }
    #else
      if (a->type != CFI_type_int)
      {
          fprintf(stderr, "Expecting object of type int_fast16_t...\n");
          printf("...exiting.\n");
          exit(17);
      }
    #endif
#else
      if (a->type != CFI_type_int_fast16_t)
      {
          fprintf(stderr, "Expecting object of type int_fast16_t...\n");
          printf("...exiting.\n");
          exit(17);
      }
#endif
      int_fast16_t *address;
      address = (int_fast16_t *) CFI_address(a, NULL);
      printf("Type is int_fast16_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 16){
#ifdef __linux__
    #ifdef __64BIT__
      if (a->type != CFI_type_long_long)
      {
          fprintf(stderr, "Expecting object of int_fast32_t...\n");
          printf("...exiting.\n");
          exit(18);
      }
    #else
      if (a->type != CFI_type_int)
      {
          fprintf(stderr, "Expecting object of type int_fast32_t...\n");
          printf("...exiting.\n");
          exit(18);
      }
    #endif
#else
      if (a->type != CFI_type_int_fast32_t)
      {
          fprintf(stderr, "Expecting object of type int_fast32_t...\n");
          printf("...exiting.\n");
          exit(18);
      }
#endif
      int_fast32_t *address;
      address = (int_fast32_t *) CFI_address(a, NULL);
      printf("Type is int_fast32_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 17){
      if (a->type != CFI_type_int_fast64_t)
      {
          fprintf(stderr, "Expecting object of type int_fast64_t...\n");
          printf("...exiting.\n");
          exit(19);
      }
      int_fast64_t *address;
      address = (int_fast64_t *) CFI_address(a, NULL);
      printf("Type is int_fast64_t, the received value is %lld\n", *address);
      fflush(stdout);
      }
   else if(flag == 18){
      if (a->type != CFI_type_intmax_t)
      {
          fprintf(stderr, "Expecting object of type intmax_t...\n");
          printf("...exiting.\n");
          exit(20);
      }
      intmax_t *address;
      address = (intmax_t *) CFI_address(a, NULL);
      printf("Type is intmax_t, the received value is %lld\n", *address);
      fflush(stdout);
      }
   else if(flag == 19){
#ifdef __64BIT__
      if (a->type != CFI_type_long_long)
      {
          fprintf(stderr, "Expecting object of type intptr_t...\n");
          printf("...exiting.\n");
          exit(21);
      }
#else
      if (a->type != CFI_type_int)
      {
          fprintf(stderr, "Expecting object of type intptr_t...\n");
          printf("...exiting.\n");
          exit(21);
      }
#endif
      
      intptr_t *address;
      address = (intptr_t *) CFI_address(a, NULL);
      printf("Type is intptr_t, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 20){
      if (a->type != CFI_type_float)
      {
          fprintf(stderr, "Expecting object of type float...\n");
          printf("...exiting.\n");
          exit(22);
      }
      float *address;
      address = (float *) CFI_address(a, NULL);
      printf("Type is float, the received value is %f\n", *address);
      fflush(stdout);
      }
   else if(flag == 21){
      if (a->type != CFI_type_double)
      {
          fprintf(stderr, "Expecting object of type double...\n");
          printf("...exiting.\n");
          exit(23);
      }
      double *address;
      address = (double *) CFI_address(a, NULL);
      printf("Type is double, the received value is %f\n", *address);
      fflush(stdout);
      }
   else if(flag == 22){
      if (a->type != CFI_type_long_double)
      {
          fprintf(stderr, "Expecting object of type long double...\n");
          printf("...exiting.\n");
          exit(24);
      }
      long double *address;
      address = (long double *) CFI_address(a, NULL);
      printf("Type is long double, the received value is %Lf\n", *address);
      fflush(stdout);
      }
   else if(flag == 23){
      if (a->type != CFI_type_float_Complex)
      {
          fprintf(stderr, "Expecting object of type float complex...\n");
          printf("...exiting.\n");
          exit(25);
      }
      float complex *address;
      address = (float complex *) CFI_address(a, NULL);
      printf("Type is float complex, the received value is (%f, %f)\n", crealf(*address), cimagf(*address));
      fflush(stdout);
      }
   else if(flag == 24){
      if (a->type != CFI_type_double_Complex)
      {
          fprintf(stderr, "Expecting object of type double complex...\n");
          printf("...exiting.\n");
          exit(26);
      }
      double complex *address;
      address = (double complex *) CFI_address(a, NULL);
      printf("Type is double complex, the received value is (%f, %f)\n", crealf(*address), cimagf(*address));
      fflush(stdout);
      }
   else if(flag == 25){
      if (a->type != CFI_type_long_double_Complex)
      {
          fprintf(stderr, "Expecting object of type long double complex...\n");
          printf("...exiting.\n");
          exit(27);
      }
      long double complex *address;
      address = (long double complex *) CFI_address(a, NULL);

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
      if (a->type != CFI_type_Bool)
      {
          fprintf(stderr, "Expecting object of type _Bool...\n");
          printf("...exiting.\n");
          exit(28);
      }
      _Bool *address;
      address = (_Bool *) CFI_address(a, NULL);
      printf("Type is _Bool, the received value is %d\n", *address);
      fflush(stdout);
      }
   else if(flag == 27){
      if (a->type != CFI_type_char)
      {
          fprintf(stderr, "Expecting object of type char...\n");
          printf("...exiting.\n");
          exit(29);
      }
      char *address;
      address = (char *) CFI_address(a, NULL);
      printf("Type is char, the received value is %c\n", *address);
      fflush(stdout);
      }
   else if(flag == 28){
#ifdef __64BIT__
      if (a->type != CFI_type_long_long)
      {
          fprintf(stderr, "Expecting object of type size_t...\n");
          printf("...exiting.\n");
          exit(30);
      }
#else
      if (a->type != CFI_type_int)
      {
          fprintf(stderr, "Expecting object of type size_t...\n");
          printf("...exiting.\n");
          exit(30);
      }
#endif
      
      size_t *address;
      address = (size_t *) CFI_address(a, NULL);
      printf("Type is size_t, the received value is %zd\n", *address);
      fflush(stdout);
      }
   else if(flag == 29){
      if (a->type != CFI_type_struct)
      {
          fprintf(stderr, "Expecting object of type struct...\n");
          printf("...exiting.\n");
          exit(31);
      }
      DT0 *address;
      address = (DT0 *) CFI_address(a, NULL);
      printf("Type is struct, the received value is %c %d\n", address->a, address->b);
      fflush(stdout);
      }
   else if(flag == 30){
      if (a->type != CFI_type_struct)
      {
          fprintf(stderr, "Expecting object of type struct...\n");
          printf("...exiting.\n");
          exit(32);
      }
      DT1 *address;
      address = (DT1 *) CFI_address(a, NULL);
      printf("Type is struct, the received value is %c %d %c %d\n", address->a, address->b, address->d0.a, address->d0.b);
      fflush(stdout);
      }

  return;
}
