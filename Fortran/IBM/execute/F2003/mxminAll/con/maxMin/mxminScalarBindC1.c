#include <stdio.h>
#include <stdlib.h>

int func1(char *cha, char *chb) {

   if ( *cha != 'c' ) exit(21);
   if ( *chb != 'd' ) exit(22);

   return 0;
}
