#include <stdio.h>
#include <stdlib.h>

char func1(char *cha, char *chb) {

   if ( *cha != 'c' ) exit(21);
   if ( *chb != 'd' ) exit(22);

   *cha = 'A';
   *chb = 'B';

   return 0;
}
