#include <stdio.h>
#include <stdlib.h>


void extsub_int(signed char **i, signed char  **j);

void extsub_real(float **i);

void extsub_log(unsigned char **i, unsigned char **j);

void extsub_char(char **i, char **j);

int main() {

   signed char *i1, *i2, i;
   
   float *r1, r;
  
   unsigned char *l1, *l2, l, ll;

   char *ch1, *ch2, c;

   i = 10;
   i2 = &i;

   r = 1.0f;
   r1 = &r;

   l = 1;
   ll = 0;
   l2 = &l;
   l1 = &ll;
   
   c = 'd';
   ch2 = &c;

   extsub_int(&i1, &i2);
   if ( *i1 != 10 ) exit(10);

   extsub_real(&r1);

   extsub_log(&l2, &l1); 
   if ( *l2 != 0 ) exit(30);
   extsub_char(&ch1, &ch2);
   if (*ch1 != 'd') exit(40);

   return 0;

} 
