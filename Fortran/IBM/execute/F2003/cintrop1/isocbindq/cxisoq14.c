/*
        C code for testcase "fxisoq14.f" and "fxisoq15.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

struct dt0 {
   long double _Complex a;
};

struct dt1 {
   long double _Complex a;
   struct dt0 d0;
};

struct dt2 {
   long double _Complex a;
   struct dt1 d1;
};

long double _Complex fnt1(struct dt0 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0l+I*5.0l ) exit(21);
#else
   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(23);
#endif

#ifdef CMPLX
   dt->a = dt->a + 5.0l+I*5.0l;
#else
   dt->a = dt->a + createcomplexl(5.0l,5.0l);
#endif

   return 0;
}

long double _Complex fnt2(struct dt0 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0l+I*5.0l ) exit(25);
#else
   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(27);
#endif

#ifdef CMPLX
   dt.a = dt.a + 5.0l+I*5.0l;
#else
   dt.a = dt.a + createcomplexl(5.0l,5.0l);
#endif

   return 0;
}

long double _Complex fnt3(struct dt1 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0l+I*5.0l ) exit(29);
#else
   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(31);
#endif
#ifdef CMPLX
   if ( dt->d0.a != 5.0l+I*5.0l ) exit(33);
#else
   if ( dt->d0.a != createcomplexl(5.0l,5.0l) ) exit(35);
#endif

#ifdef CMPLX
   dt->a = dt->a + 5.0l+I*5.0l;
#else
   dt->a = dt->a + createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dt->d0.a = dt->d0.a + 5.0l+I*5.0l;
#else
   dt->d0.a = dt->d0.a + createcomplexl(5.0l,5.0l);
#endif

   return 0;
}

long double _Complex fnt4(struct dt1 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0l+I*5.0l ) exit(37);
#else
   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(39);
#endif
#ifdef CMPLX
   if ( dt.d0.a != 5.0l+I*5.0l ) exit(41);
#else
   if ( dt.d0.a != createcomplexl(5.0l,5.0l) ) exit(43);
#endif

#ifdef CMPLX
   dt.a = dt.a + 5.0l+I*5.0l;
#else
   dt.a = dt.a + createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dt.d0.a = dt.d0.a + 5.0l+I*5.0l;
#else
   dt.d0.a = dt.d0.a + createcomplexl(5.0l,5.0l);
#endif

   return 0;
}

long double _Complex fnt5(struct dt2 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0l+I*5.0l ) exit(45);
#else
   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(47);
#endif
#ifdef CMPLX
   if ( dt->d1.a != 5.0l+I*5.0l ) exit(49);
#else
   if ( dt->d1.a != createcomplexl(5.0l,5.0l) ) exit(51);
#endif
#ifdef CMPLX
   if ( dt->d1.d0.a != 5.0l+I*5.0l ) exit(53);
#else
   if ( dt->d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(55);
#endif

#ifdef CMPLX
   dt->a = dt->a + 5.0l+I*5.0l;
#else
   dt->a = dt->a + createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dt->d1.a = dt->d1.a + 5.0l+I*5.0l;
#else
   dt->d1.a = dt->d1.a + createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dt->d1.d0.a = dt->d1.d0.a + 5.0l+I*5.0l;
#else
   dt->d1.d0.a = dt->d1.d0.a + createcomplexl(5.0l,5.0l);
#endif

   return 0;
}

long double _Complex fnt6(struct dt2 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0l+I*5.0l ) exit(57);
#else
   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(59);
#endif
#ifdef CMPLX
   if ( dt.d1.a != 5.0l+I*5.0l ) exit(61);
#else
   if ( dt.d1.a != createcomplexl(5.0l,5.0l) ) exit(63);
#endif
#ifdef CMPLX
   if ( dt.d1.d0.a != 5.0l+I*5.0l ) exit(65);
#else
   if ( dt.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(67);
#endif

#ifdef CMPLX
   dt.a = dt.a + 5.0l+I*5.0l;
#else
   dt.a = dt.a + createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dt.d1.a = dt.d1.a + 5.0l+I*5.0l;
#else
   dt.d1.a = dt.d1.a + createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dt.d1.d0.a = dt.d1.d0.a + 5.0l+I*5.0l;
#else
   dt.d1.d0.a = dt.d1.d0.a + createcomplexl(5.0l,5.0l);
#endif

   return 0;
}

long double _Complex fnt7(struct dt0 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0l+I*5.0l ) exit(69);
#else
   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(71);
#endif

   return 0;
}

long double _Complex fnt7a(const struct dt0 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0l+I*5.0l ) exit(73);
#else
   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(75);
#endif

   return 0;
}

long double _Complex fnt8(struct dt0 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0l+I*5.0l ) exit(77);
#else
   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(79);
#endif

   return 0;
}

long double _Complex fnt8a(const struct dt0 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0l+I*5.0l ) exit(81);
#else
   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(83);
#endif

   return 0;
}

long double _Complex fnt9(struct dt1 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0l+I*5.0l ) exit(85);
#else
   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(87);
#endif
#ifdef CMPLX
   if ( dt->d0.a != 5.0l+I*5.0l ) exit(89);
#else
   if ( dt->d0.a != createcomplexl(5.0l,5.0l) ) exit(91);
#endif

   return 0;
}

long double _Complex fnt9a(const struct dt1 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0l+I*5.0l ) exit(93);
#else
   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(95);
#endif
#ifdef CMPLX
   if ( dt->d0.a != 5.0l+I*5.0l ) exit(97);
#else
   if ( dt->d0.a != createcomplexl(5.0l,5.0l) ) exit(99);
#endif

   return 0;
}

long double _Complex fnt10(struct dt1 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0l+I*5.0l ) exit(101);
#else
   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(103);
#endif
#ifdef CMPLX
   if ( dt.d0.a != 5.0l+I*5.0l ) exit(105);
#else
   if ( dt.d0.a != createcomplexl(5.0l,5.0l) ) exit(107);
#endif

   return 0;
}

long double _Complex fnt10a(const struct dt1 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0l+I*5.0l ) exit(109);
#else
   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(111);
#endif
#ifdef CMPLX
   if ( dt.d0.a != 5.0l+I*5.0l ) exit(113);
#else
   if ( dt.d0.a != createcomplexl(5.0l,5.0l) ) exit(115);
#endif

   return 0;
}

long double _Complex fnt11(struct dt2 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0l+I*5.0l ) exit(117);
#else
   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(119);
#endif
#ifdef CMPLX
   if ( dt->d1.a != 5.0l+I*5.0l ) exit(121);
#else
   if ( dt->d1.a != createcomplexl(5.0l,5.0l) ) exit(123);
#endif
#ifdef CMPLX
   if ( dt->d1.d0.a != 5.0l+I*5.0l ) exit(125);
#else
   if ( dt->d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(127);
#endif

   return 0;
}

long double _Complex fnt11a(const struct dt2 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0l+I*5.0l ) exit(129);
#else
   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(131);
#endif
#ifdef CMPLX
   if ( dt->d1.a != 5.0l+I*5.0l ) exit(133);
#else
   if ( dt->d1.a != createcomplexl(5.0l,5.0l) ) exit(135);
#endif
#ifdef CMPLX
   if ( dt->d1.d0.a != 5.0l+I*5.0l ) exit(137);
#else
   if ( dt->d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(141);
#endif

   return 0;
}

long double _Complex fnt12(struct dt2 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0l+I*5.0l ) exit(143);
#else
   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(145);
#endif
#ifdef CMPLX
   if ( dt.d1.a != 5.0l+I*5.0l ) exit(147);
#else
   if ( dt.d1.a != createcomplexl(5.0l,5.0l) ) exit(149);
#endif
#ifdef CMPLX
   if ( dt.d1.d0.a != 5.0l+I*5.0l ) exit(151);
#else
   if ( dt.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(153);
#endif

   return 0;
}

long double _Complex fnt12a(const struct dt2 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0l+I*5.0l ) exit(155);
#else
   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(157);
#endif
#ifdef CMPLX
   if ( dt.d1.a != 5.0l+I*5.0l ) exit(159);
#else
   if ( dt.d1.a != createcomplexl(5.0l,5.0l) ) exit(161);
#endif
#ifdef CMPLX
   if ( dt.d1.d0.a != 5.0l+I*5.0l ) exit(163);
#else
   if ( dt.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(165);
#endif

   return 0;
}

long double _Complex fnt13(struct dt0 *dt) {

#ifdef CMPLX
   dt->a = dt->a + 5.0l+I*5.0l;
#else
   dt->a = dt->a + createcomplexl(5.0l,5.0l);
#endif

   return 0;
}

long double _Complex fnt14(struct dt1 *dt) {

#ifdef CMPLX
   dt->a = dt->a + 5.0l+I*5.0l;
#else
   dt->a = dt->a + createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dt->d0.a = dt->d0.a + 5.0l+I*5.0l;
#else
   dt->d0.a = dt->d0.a + createcomplexl(5.0l,5.0l);
#endif

   return 0;
}

long double _Complex fnt15(struct dt2 *dt) {

#ifdef CMPLX
   dt->a = dt->a + 5.0l+I*5.0l;
#else
   dt->a = dt->a + createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dt->d1.a = dt->d1.a + 5.0l+I*5.0l;
#else
   dt->d1.a = dt->d1.a + createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dt->d1.d0.a = dt->d1.d0.a + 5.0l+I*5.0l;
#else
   dt->d1.d0.a = dt->d1.d0.a + createcomplexl(5.0l,5.0l);
#endif

   return 0;
}

