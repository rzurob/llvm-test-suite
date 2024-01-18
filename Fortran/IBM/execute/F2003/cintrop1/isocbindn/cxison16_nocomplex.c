/*
        C code for testcase "fxison16.f" and "fxison17.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

struct dt0 {
   float _Complex a;
   double _Complex b;
};

struct dt1 {
   float _Complex a;
   double _Complex b;
   struct dt0 d0;
};

struct dt2 {
   float _Complex a;
   double _Complex b;
   struct dt1 d1;
};

void sub1(struct dt0 *dt) {

   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(23);
   if ( dt->b != createcomplex(10.0,10.0) ) exit(27);

   dt->a = dt->a + createcomplexf(5.0f,5.0f);
   dt->b = dt->b + createcomplex(10.0,10.0);

}

void sub2(struct dt0 dt) {

   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(31);
   if ( dt.b != createcomplex(10.0,10.0) ) exit(35);

   dt.a = dt.a + createcomplexf(5.0f,5.0f);
   dt.b = dt.b + createcomplex(10.0,10.0);

}

void sub3(struct dt1 *dt) {

   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(39);
   if ( dt->b != createcomplex(10.0,10.0) ) exit(43);
   if ( dt->d0.a != createcomplexf(5.0f,5.0f) ) exit(47);
   if ( dt->d0.b != createcomplex(10.0,10.0) ) exit(51);

   dt->a = dt->a + createcomplexf(5.0f,5.0f);
   dt->b = dt->b + createcomplex(10.0,10.0);
   dt->d0.a = dt->d0.a + createcomplexf(5.0f,5.0f);
   dt->d0.b = dt->d0.b + createcomplex(10.0,10.0);

}

void sub4(struct dt1 dt) {

   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(55);
   if ( dt.b != createcomplex(10.0,10.0) ) exit(59);
   if ( dt.d0.a != createcomplexf(5.0f,5.0f) ) exit(63);
   if ( dt.d0.b != createcomplex(10.0,10.0) ) exit(67);

   dt.a = dt.a + createcomplexf(5.0f,5.0f);
   dt.b = dt.b + createcomplex(10.0,10.0);
   dt.d0.a = dt.d0.a + createcomplexf(5.0f,5.0f);
   dt.d0.b = dt.d0.b + createcomplex(10.0,10.0);

}

void sub5(struct dt2 *dt) {

   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(71);
   if ( dt->b != createcomplex(10.0,10.0) ) exit(75);
   if ( dt->d1.a != createcomplexf(5.0f,5.0f) ) exit(79);
   if ( dt->d1.b != createcomplex(10.0,10.0) ) exit(83);
   if ( dt->d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(87);
   if ( dt->d1.d0.b != createcomplex(10.0,10.0) ) exit(91);

   dt->a = dt->a + createcomplexf(5.0f,5.0f);
   dt->b = dt->b + createcomplex(10.0,10.0);
   dt->d1.a = dt->d1.a + createcomplexf(5.0f,5.0f);
   dt->d1.b = dt->d1.b + createcomplex(10.0,10.0);
   dt->d1.d0.a = dt->d1.d0.a + createcomplexf(5.0f,5.0f);
   dt->d1.d0.b = dt->d1.d0.b + createcomplex(10.0,10.0);

}

void sub6(struct dt2 dt) {

   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(95);
   if ( dt.b != createcomplex(10.0,10.0) ) exit(99);
   if ( dt.d1.a != createcomplexf(5.0f,5.0f) ) exit(103);
   if ( dt.d1.b != createcomplex(10.0,10.0) ) exit(107);
   if ( dt.d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(111);
   if ( dt.d1.d0.b != createcomplex(10.0,10.0) ) exit(115);

   dt.a = dt.a + createcomplexf(5.0f,5.0f);
   dt.b = dt.b + createcomplex(10.0,10.0);
   dt.d1.a = dt.d1.a + createcomplexf(5.0f,5.0f);
   dt.d1.b = dt.d1.b + createcomplex(10.0,10.0);
   dt.d1.d0.a = dt.d1.d0.a + createcomplexf(5.0f,5.0f);
   dt.d1.d0.b = dt.d1.d0.b + createcomplex(10.0,10.0);

}

void sub7(struct dt0 *dt) {

   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(119);
   if ( dt->b != createcomplex(10.0,10.0) ) exit(123);

}

void sub7a(const struct dt0 *dt) {

   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(127);
   if ( dt->b != createcomplex(10.0,10.0) ) exit(131);

}

void sub8(struct dt0 dt) {

   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(135);
   if ( dt.b != createcomplex(10.0,10.0) ) exit(141);

}

void sub8a(const struct dt0 dt) {

   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(145);
   if ( dt.b != createcomplex(10.0,10.0) ) exit(149);

}

void sub9(struct dt1 *dt) {

   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(153);
   if ( dt->b != createcomplex(10.0,10.0) ) exit(157);
   if ( dt->d0.a != createcomplexf(5.0f,5.0f) ) exit(161);
   if ( dt->d0.b != createcomplex(10.0,10.0) ) exit(165);

}

void sub9a(const struct dt1 *dt) {

   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(169);
   if ( dt->b != createcomplex(10.0,10.0) ) exit(173);
   if ( dt->d0.a != createcomplexf(5.0f,5.0f) ) exit(177);
   if ( dt->d0.b != createcomplex(10.0,10.0) ) exit(181);

}

void sub10(struct dt1 dt) {

   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(185);
   if ( dt.b != createcomplex(10.0,10.0) ) exit(189);
   if ( dt.d0.a != createcomplexf(5.0f,5.0f) ) exit(193);
   if ( dt.d0.b != createcomplex(10.0,10.0) ) exit(197);

}

void sub10a(const struct dt1 dt) {

   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(201);
   if ( dt.b != createcomplex(10.0,10.0) ) exit(205);
   if ( dt.d0.a != createcomplexf(5.0f,5.0f) ) exit(209);
   if ( dt.d0.b != createcomplex(10.0,10.0) ) exit(213);

}

void sub11(struct dt2 *dt) {

   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(217);
   if ( dt->b != createcomplex(10.0,10.0) ) exit(221);
   if ( dt->d1.a != createcomplexf(5.0f,5.0f) ) exit(225);
   if ( dt->d1.b != createcomplex(10.0,10.0) ) exit(229);
   if ( dt->d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(233);
   if ( dt->d1.d0.b != createcomplex(10.0,10.0) ) exit(237);

}

void sub11a(const struct dt2 *dt) {

   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(241);
   if ( dt->b != createcomplex(10.0,10.0) ) exit(245);
   if ( dt->d1.a != createcomplexf(5.0f,5.0f) ) exit(249);
   if ( dt->d1.b != createcomplex(10.0,10.0) ) exit(255);
   if ( dt->d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(259);
   if ( dt->d1.d0.b != createcomplex(10.0,10.0) ) exit(263);

}

void sub12(struct dt2 dt) {

   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(267);
   if ( dt.b != createcomplex(10.0,10.0) ) exit(271);
   if ( dt.d1.a != createcomplexf(5.0f,5.0f) ) exit(275);
   if ( dt.d1.b != createcomplex(10.0,10.0) ) exit(279);
   if ( dt.d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(283);
   if ( dt.d1.d0.b != createcomplex(10.0,10.0) ) exit(287);

}

void sub12a(const struct dt2 dt) {

   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(291);
   if ( dt.b != createcomplex(10.0,10.0) ) exit(295);
   if ( dt.d1.a != createcomplexf(5.0f,5.0f) ) exit(299);
   if ( dt.d1.b != createcomplex(10.0,10.0) ) exit(303);
   if ( dt.d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(307);
   if ( dt.d1.d0.b != createcomplex(10.0,10.0) ) exit(311);

}

void sub13(struct dt0 *dt) {

   dt->a = dt->a + createcomplexf(5.0f,5.0f);
   dt->b = dt->b + createcomplex(10.0,10.0);

}

void sub14(struct dt1 *dt) {

   dt->a = dt->a + createcomplexf(5.0f,5.0f);
   dt->b = dt->b + createcomplex(10.0,10.0);
   dt->d0.a = dt->d0.a + createcomplexf(5.0f,5.0f);
   dt->d0.b = dt->d0.b + createcomplex(10.0,10.0);

}

void sub15(struct dt2 *dt) {

   dt->a = dt->a + createcomplexf(5.0f,5.0f);
   dt->b = dt->b + createcomplex(10.0,10.0);
   dt->d1.a = dt->d1.a + createcomplexf(5.0f,5.0f);
   dt->d1.b = dt->d1.b + createcomplex(10.0,10.0);
   dt->d1.d0.a = dt->d1.d0.a + createcomplexf(5.0f,5.0f);
   dt->d1.d0.b = dt->d1.d0.b + createcomplex(10.0,10.0);

}

