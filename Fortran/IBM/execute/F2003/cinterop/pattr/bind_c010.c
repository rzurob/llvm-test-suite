#include <stdio.h>
#include <stdlib.h>

void extsub_int(signed char *i1, signed long long *i2);

void extsub_log(unsigned char *l1);

void extsub_char(char *ch1);

int main() {

   signed char i1 = 0;
   signed long long i2 = 0;

   unsigned char l1 = 0;

   char ch1 = 'd';

   extsub_int(&i1, &i2);
   if ( i1 != 15 ) exit(10);
   if ( i2 != 15 ) exit(11);

   extsub_log(&l1);
   if ( !l1 ) exit(12);

   extsub_char(&ch1);
   if ( ch1 != 'a') exit(13);

   return 0;

}

