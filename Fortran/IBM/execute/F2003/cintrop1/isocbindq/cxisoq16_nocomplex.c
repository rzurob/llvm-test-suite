/*
        C code for testcase "fxisoq16.f" and "fxisoq17.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

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

void sub1(struct dt0 *dt) {

   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(23);

   dt->a = dt->a + createcomplexl(5.0l,5.0l);

}

void sub2(struct dt0 dt) {

   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(27);

   dt.a = dt.a + createcomplexl(5.0l,5.0l);

}

void sub3(struct dt1 *dt) {

   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(31);
   if ( dt->d0.a != createcomplexl(5.0l,5.0l) ) exit(35);

   dt->a = dt->a + createcomplexl(5.0l,5.0l);
   dt->d0.a = dt->d0.a + createcomplexl(5.0l,5.0l);

}

void sub4(struct dt1 dt) {

   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(39);
   if ( dt.d0.a != createcomplexl(5.0l,5.0l) ) exit(43);

   dt.a = dt.a + createcomplexl(5.0l,5.0l);
   dt.d0.a = dt.d0.a + createcomplexl(5.0l,5.0l);

}

void sub5(struct dt2 *dt) {

   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(47);
   if ( dt->d1.a != createcomplexl(5.0l,5.0l) ) exit(51);
   if ( dt->d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(55);

   dt->a = dt->a + createcomplexl(5.0l,5.0l);
   dt->d1.a = dt->d1.a + createcomplexl(5.0l,5.0l);
   dt->d1.d0.a = dt->d1.d0.a + createcomplexl(5.0l,5.0l);

}

void sub6(struct dt2 dt) {

   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(59);
   if ( dt.d1.a != createcomplexl(5.0l,5.0l) ) exit(63);
   if ( dt.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(67);

   dt.a = dt.a + createcomplexl(5.0l,5.0l);
   dt.d1.a = dt.d1.a + createcomplexl(5.0l,5.0l);
   dt.d1.d0.a = dt.d1.d0.a + createcomplexl(5.0l,5.0l);

}

void sub7(struct dt0 *dt) {

   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(71);

}

void sub7a(const struct dt0 *dt) {

   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(75);

}

void sub8(struct dt0 dt) {

   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(79);

}

void sub8a(const struct dt0 dt) {

   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(83);

}

void sub9(struct dt1 *dt) {

   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(87);
   if ( dt->d0.a != createcomplexl(5.0l,5.0l) ) exit(91);

}

void sub9a(const struct dt1 *dt) {

   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(95);
   if ( dt->d0.a != createcomplexl(5.0l,5.0l) ) exit(99);

}

void sub10(struct dt1 dt) {

   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(103);
   if ( dt.d0.a != createcomplexl(5.0l,5.0l) ) exit(107);

}

void sub10a(const struct dt1 dt) {

   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(111);
   if ( dt.d0.a != createcomplexl(5.0l,5.0l) ) exit(115);

}

void sub11(struct dt2 *dt) {

   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(119);
   if ( dt->d1.a != createcomplexl(5.0l,5.0l) ) exit(123);
   if ( dt->d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(127);

}

void sub11a(const struct dt2 *dt) {

   if ( dt->a != createcomplexl(5.0l,5.0l) ) exit(131);
   if ( dt->d1.a != createcomplexl(5.0l,5.0l) ) exit(135);
   if ( dt->d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(141);

}

void sub12(struct dt2 dt) {

   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(145);
   if ( dt.d1.a != createcomplexl(5.0l,5.0l) ) exit(149);
   if ( dt.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(153);

}

void sub12a(const struct dt2 dt) {

   if ( dt.a != createcomplexl(5.0l,5.0l) ) exit(157);
   if ( dt.d1.a != createcomplexl(5.0l,5.0l) ) exit(161);
   if ( dt.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(165);

}

void sub13(struct dt0 *dt) {

   dt->a = dt->a + createcomplexl(5.0l,5.0l);

}

void sub14(struct dt1 *dt) {

   dt->a = dt->a + createcomplexl(5.0l,5.0l);
   dt->d0.a = dt->d0.a + createcomplexl(5.0l,5.0l);

}

void sub15(struct dt2 *dt) {

   dt->a = dt->a + createcomplexl(5.0l,5.0l);
   dt->d1.a = dt->d1.a + createcomplexl(5.0l,5.0l);
   dt->d1.d0.a = dt->d1.d0.a + createcomplexl(5.0l,5.0l);

}

