
/*
        C code for testcase "fxisor02.f" and "fxisor03.f"
*/

#include <stdio.h>
#include <stdlib.h>


_Bool fnt1(_Bool *a) {

   if ( *a != 1 ) exit(21);

   *a = 0;

   return 0;
}

_Bool fnt2(_Bool a) {

   if ( a != 1 ) exit(23);

   a = 0;

   return 0;
}

_Bool fnt3(_Bool *a) {

   if ( *a != 1 ) exit(25);

   return 0;
}

_Bool fnt4(_Bool a) {

   if ( a != 1 ) exit(27);

   return 0;
}

_Bool fnt5(const _Bool *a) {

   if ( *a != 1 ) exit(29);

   return 0;
}

_Bool fnt6(const _Bool a) {

   if ( a != 1 ) exit(31);

   return 0;
}
