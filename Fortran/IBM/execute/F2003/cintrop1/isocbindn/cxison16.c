/*
        C code for testcase "fxison16.f" and "fxison17.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

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

#ifdef CMPLX
   if ( dt->a != 5.0f+I*5.0f ) exit(21);
#else
   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(23);
#endif
#ifdef CMPLX
   if ( dt->b != 10.0+I*10.0 ) exit(25);
#else
   if ( dt->b != createcomplex(10.0,10.0) ) exit(27);
#endif

#ifdef CMPLX
   dt->a = dt->a + 5.0f+I*5.0f;
#else
   dt->a = dt->a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt->b = dt->b + 10.0+I*10.0;
#else
   dt->b = dt->b + createcomplex(10.0,10.0);
#endif

}

void sub2(struct dt0 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0f+I*5.0f ) exit(29);
#else
   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(31);
#endif
#ifdef CMPLX
   if ( dt.b != 10.0+I*10.0 ) exit(33);
#else
   if ( dt.b != createcomplex(10.0,10.0) ) exit(35);
#endif

#ifdef CMPLX
   dt.a = dt.a + 5.0f+I*5.0f;
#else
   dt.a = dt.a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt.b = dt.b + 10.0+I*10.0;
#else
   dt.b = dt.b + createcomplex(10.0,10.0);
#endif

}

void sub3(struct dt1 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0f+I*5.0f ) exit(37);
#else
   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(39);
#endif
#ifdef CMPLX
   if ( dt->b != 10.0+I*10.0 ) exit(41);
#else
   if ( dt->b != createcomplex(10.0,10.0) ) exit(43);
#endif
#ifdef CMPLX
   if ( dt->d0.a != 5.0f+I*5.0f ) exit(45);
#else
   if ( dt->d0.a != createcomplexf(5.0f,5.0f) ) exit(47);
#endif
#ifdef CMPLX
   if ( dt->d0.b != 10.0+I*10.0 ) exit(49);
#else
   if ( dt->d0.b != createcomplex(10.0,10.0) ) exit(51);
#endif

#ifdef CMPLX
   dt->a = dt->a + 5.0f+I*5.0f;
#else
   dt->a = dt->a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt->b = dt->b + 10.0+I*10.0;
#else
   dt->b = dt->b + createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dt->d0.a = dt->d0.a + 5.0f+I*5.0f;
#else
   dt->d0.a = dt->d0.a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt->d0.b = dt->d0.b + 10.0+I*10.0;
#else
   dt->d0.b = dt->d0.b + createcomplex(10.0,10.0);
#endif

}

void sub4(struct dt1 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0f+I*5.0f ) exit(53);
#else
   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(55);
#endif
#ifdef CMPLX
   if ( dt.b != 10.0+I*10.0 ) exit(57);
#else
   if ( dt.b != createcomplex(10.0,10.0) ) exit(59);
#endif
#ifdef CMPLX
   if ( dt.d0.a != 5.0f+I*5.0f ) exit(61);
#else
   if ( dt.d0.a != createcomplexf(5.0f,5.0f) ) exit(63);
#endif
#ifdef CMPLX
   if ( dt.d0.b != 10.0+I*10.0 ) exit(65);
#else
   if ( dt.d0.b != createcomplex(10.0,10.0) ) exit(67);
#endif

#ifdef CMPLX
   dt.a = dt.a + 5.0f+I*5.0f;
#else
   dt.a = dt.a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt.b = dt.b + 10.0+I*10.0;
#else
   dt.b = dt.b + createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dt.d0.a = dt.d0.a + 5.0f+I*5.0f;
#else
   dt.d0.a = dt.d0.a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt.d0.b = dt.d0.b + 10.0+I*10.0;
#else
   dt.d0.b = dt.d0.b + createcomplex(10.0,10.0);
#endif

}

void sub5(struct dt2 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0f+I*5.0f ) exit(69);
#else
   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(71);
#endif
#ifdef CMPLX
   if ( dt->b != 10.0+I*10.0 ) exit(73);
#else
   if ( dt->b != createcomplex(10.0,10.0) ) exit(75);
#endif
#ifdef CMPLX
   if ( dt->d1.a != 5.0f+I*5.0f ) exit(77);
#else
   if ( dt->d1.a != createcomplexf(5.0f,5.0f) ) exit(79);
#endif
#ifdef CMPLX
   if ( dt->d1.b != 10.0+I*10.0 ) exit(81);
#else
   if ( dt->d1.b != createcomplex(10.0,10.0) ) exit(83);
#endif
#ifdef CMPLX
   if ( dt->d1.d0.a != 5.0f+I*5.0f ) exit(85);
#else
   if ( dt->d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(87);
#endif
#ifdef CMPLX
   if ( dt->d1.d0.b != 10.0+I*10.0 ) exit(89);
#else
   if ( dt->d1.d0.b != createcomplex(10.0,10.0) ) exit(91);
#endif

#ifdef CMPLX
   dt->a = dt->a + 5.0f+I*5.0f;
#else
   dt->a = dt->a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt->b = dt->b + 10.0+I*10.0;
#else
   dt->b = dt->b + createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dt->d1.a = dt->d1.a + 5.0f+I*5.0f;
#else
   dt->d1.a = dt->d1.a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt->d1.b = dt->d1.b + 10.0+I*10.0;
#else
   dt->d1.b = dt->d1.b + createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dt->d1.d0.a = dt->d1.d0.a + 5.0f+I*5.0f;
#else
   dt->d1.d0.a = dt->d1.d0.a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt->d1.d0.b = dt->d1.d0.b + 10.0+I*10.0;
#else
   dt->d1.d0.b = dt->d1.d0.b + createcomplex(10.0,10.0);
#endif

}

void sub6(struct dt2 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0f+I*5.0f ) exit(93);
#else
   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(95);
#endif
#ifdef CMPLX
   if ( dt.b != 10.0+I*10.0 ) exit(97);
#else
   if ( dt.b != createcomplex(10.0,10.0) ) exit(99);
#endif
#ifdef CMPLX
   if ( dt.d1.a != 5.0f+I*5.0f ) exit(101);
#else
   if ( dt.d1.a != createcomplexf(5.0f,5.0f) ) exit(103);
#endif
#ifdef CMPLX
   if ( dt.d1.b != 10.0+I*10.0 ) exit(105);
#else
   if ( dt.d1.b != createcomplex(10.0,10.0) ) exit(107);
#endif
#ifdef CMPLX
   if ( dt.d1.d0.a != 5.0f+I*5.0f ) exit(109);
#else
   if ( dt.d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(111);
#endif
#ifdef CMPLX
   if ( dt.d1.d0.b != 10.0+I*10.0 ) exit(113);
#else
   if ( dt.d1.d0.b != createcomplex(10.0,10.0) ) exit(115);
#endif

#ifdef CMPLX
   dt.a = dt.a + 5.0f+I*5.0f;
#else
   dt.a = dt.a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt.b = dt.b + 10.0+I*10.0;
#else
   dt.b = dt.b + createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dt.d1.a = dt.d1.a + 5.0f+I*5.0f;
#else
   dt.d1.a = dt.d1.a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt.d1.b = dt.d1.b + 10.0+I*10.0;
#else
   dt.d1.b = dt.d1.b + createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dt.d1.d0.a = dt.d1.d0.a + 5.0f+I*5.0f;
#else
   dt.d1.d0.a = dt.d1.d0.a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt.d1.d0.b = dt.d1.d0.b + 10.0+I*10.0;
#else
   dt.d1.d0.b = dt.d1.d0.b + createcomplex(10.0,10.0);
#endif

}

void sub7(struct dt0 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0f+I*5.0f ) exit(117);
#else
   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(119);
#endif
#ifdef CMPLX
   if ( dt->b != 10.0+I*10.0 ) exit(121);
#else
   if ( dt->b != createcomplex(10.0,10.0) ) exit(123);
#endif

}

void sub7a(const struct dt0 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0f+I*5.0f ) exit(125);
#else
   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(127);
#endif
#ifdef CMPLX
   if ( dt->b != 10.0+I*10.0 ) exit(129);
#else
   if ( dt->b != createcomplex(10.0,10.0) ) exit(131);
#endif

}

void sub8(struct dt0 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0f+I*5.0f ) exit(133);
#else
   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(135);
#endif
#ifdef CMPLX
   if ( dt.b != 10.0+I*10.0 ) exit(137);
#else
   if ( dt.b != createcomplex(10.0,10.0) ) exit(141);
#endif

}

void sub8a(const struct dt0 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0f+I*5.0f ) exit(143);
#else
   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(145);
#endif
#ifdef CMPLX
   if ( dt.b != 10.0+I*10.0 ) exit(147);
#else
   if ( dt.b != createcomplex(10.0,10.0) ) exit(149);
#endif

}

void sub9(struct dt1 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0f+I*5.0f ) exit(151);
#else
   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(153);
#endif
#ifdef CMPLX
   if ( dt->b != 10.0+I*10.0 ) exit(155);
#else
   if ( dt->b != createcomplex(10.0,10.0) ) exit(157);
#endif
#ifdef CMPLX
   if ( dt->d0.a != 5.0f+I*5.0f ) exit(159);
#else
   if ( dt->d0.a != createcomplexf(5.0f,5.0f) ) exit(161);
#endif
#ifdef CMPLX
   if ( dt->d0.b != 10.0+I*10.0 ) exit(163);
#else
   if ( dt->d0.b != createcomplex(10.0,10.0) ) exit(165);
#endif

}

void sub9a(const struct dt1 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0f+I*5.0f ) exit(167);
#else
   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(169);
#endif
#ifdef CMPLX
   if ( dt->b != 10.0+I*10.0 ) exit(171);
#else
   if ( dt->b != createcomplex(10.0,10.0) ) exit(173);
#endif
#ifdef CMPLX
   if ( dt->d0.a != 5.0f+I*5.0f ) exit(175);
#else
   if ( dt->d0.a != createcomplexf(5.0f,5.0f) ) exit(177);
#endif
#ifdef CMPLX
   if ( dt->d0.b != 10.0+I*10.0 ) exit(179);
#else
   if ( dt->d0.b != createcomplex(10.0,10.0) ) exit(181);
#endif

}

void sub10(struct dt1 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0f+I*5.0f ) exit(183);
#else
   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(185);
#endif
#ifdef CMPLX
   if ( dt.b != 10.0+I*10.0 ) exit(187);
#else
   if ( dt.b != createcomplex(10.0,10.0) ) exit(189);
#endif
#ifdef CMPLX
   if ( dt.d0.a != 5.0f+I*5.0f ) exit(191);
#else
   if ( dt.d0.a != createcomplexf(5.0f,5.0f) ) exit(193);
#endif
#ifdef CMPLX
   if ( dt.d0.b != 10.0+I*10.0 ) exit(195);
#else
   if ( dt.d0.b != createcomplex(10.0,10.0) ) exit(197);
#endif

}

void sub10a(const struct dt1 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0f+I*5.0f ) exit(199);
#else
   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(201);
#endif
#ifdef CMPLX
   if ( dt.b != 10.0+I*10.0 ) exit(203);
#else
   if ( dt.b != createcomplex(10.0,10.0) ) exit(205);
#endif
#ifdef CMPLX
   if ( dt.d0.a != 5.0f+I*5.0f ) exit(207);
#else
   if ( dt.d0.a != createcomplexf(5.0f,5.0f) ) exit(209);
#endif
#ifdef CMPLX
   if ( dt.d0.b != 10.0+I*10.0 ) exit(211);
#else
   if ( dt.d0.b != createcomplex(10.0,10.0) ) exit(213);
#endif

}

void sub11(struct dt2 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0f+I*5.0f ) exit(215);
#else
   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(217);
#endif
#ifdef CMPLX
   if ( dt->b != 10.0+I*10.0 ) exit(219);
#else
   if ( dt->b != createcomplex(10.0,10.0) ) exit(221);
#endif
#ifdef CMPLX
   if ( dt->d1.a != 5.0f+I*5.0f ) exit(223);
#else
   if ( dt->d1.a != createcomplexf(5.0f,5.0f) ) exit(225);
#endif
#ifdef CMPLX
   if ( dt->d1.b != 10.0+I*10.0 ) exit(227);
#else
   if ( dt->d1.b != createcomplex(10.0,10.0) ) exit(229);
#endif
#ifdef CMPLX
   if ( dt->d1.d0.a != 5.0f+I*5.0f ) exit(231);
#else
   if ( dt->d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(233);
#endif
#ifdef CMPLX
   if ( dt->d1.d0.b != 10.0+I*10.0 ) exit(235);
#else
   if ( dt->d1.d0.b != createcomplex(10.0,10.0) ) exit(237);
#endif

}

void sub11a(const struct dt2 *dt) {

#ifdef CMPLX
   if ( dt->a != 5.0f+I*5.0f ) exit(239);
#else
   if ( dt->a != createcomplexf(5.0f,5.0f) ) exit(241);
#endif
#ifdef CMPLX
   if ( dt->b != 10.0+I*10.0 ) exit(243);
#else
   if ( dt->b != createcomplex(10.0,10.0) ) exit(245);
#endif
#ifdef CMPLX
   if ( dt->d1.a != 5.0f+I*5.0f ) exit(247);
#else
   if ( dt->d1.a != createcomplexf(5.0f,5.0f) ) exit(249);
#endif
#ifdef CMPLX
   if ( dt->d1.b != 10.0+I*10.0 ) exit(253);
#else
   if ( dt->d1.b != createcomplex(10.0,10.0) ) exit(255);
#endif
#ifdef CMPLX
   if ( dt->d1.d0.a != 5.0f+I*5.0f ) exit(257);
#else
   if ( dt->d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(259);
#endif
#ifdef CMPLX
   if ( dt->d1.d0.b != 10.0+I*10.0 ) exit(261);
#else
   if ( dt->d1.d0.b != createcomplex(10.0,10.0) ) exit(263);
#endif

}

void sub12(struct dt2 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0f+I*5.0f ) exit(265);
#else
   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(267);
#endif
#ifdef CMPLX
   if ( dt.b != 10.0+I*10.0 ) exit(269);
#else
   if ( dt.b != createcomplex(10.0,10.0) ) exit(271);
#endif
#ifdef CMPLX
   if ( dt.d1.a != 5.0f+I*5.0f ) exit(273);
#else
   if ( dt.d1.a != createcomplexf(5.0f,5.0f) ) exit(275);
#endif
#ifdef CMPLX
   if ( dt.d1.b != 10.0+I*10.0 ) exit(277);
#else
   if ( dt.d1.b != createcomplex(10.0,10.0) ) exit(279);
#endif
#ifdef CMPLX
   if ( dt.d1.d0.a != 5.0f+I*5.0f ) exit(281);
#else
   if ( dt.d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(283);
#endif
#ifdef CMPLX
   if ( dt.d1.d0.b != 10.0+I*10.0 ) exit(285);
#else
   if ( dt.d1.d0.b != createcomplex(10.0,10.0) ) exit(287);
#endif

}

void sub12a(const struct dt2 dt) {

#ifdef CMPLX
   if ( dt.a != 5.0f+I*5.0f ) exit(289);
#else
   if ( dt.a != createcomplexf(5.0f,5.0f) ) exit(291);
#endif
#ifdef CMPLX
   if ( dt.b != 10.0+I*10.0 ) exit(293);
#else
   if ( dt.b != createcomplex(10.0,10.0) ) exit(295);
#endif
#ifdef CMPLX
   if ( dt.d1.a != 5.0f+I*5.0f ) exit(297);
#else
   if ( dt.d1.a != createcomplexf(5.0f,5.0f) ) exit(299);
#endif
#ifdef CMPLX
   if ( dt.d1.b != 10.0+I*10.0 ) exit(301);
#else
   if ( dt.d1.b != createcomplex(10.0,10.0) ) exit(303);
#endif
#ifdef CMPLX
   if ( dt.d1.d0.a != 5.0f+I*5.0f ) exit(305);
#else
   if ( dt.d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(307);
#endif
#ifdef CMPLX
   if ( dt.d1.d0.b != 10.0+I*10.0 ) exit(309);
#else
   if ( dt.d1.d0.b != createcomplex(10.0,10.0) ) exit(311);
#endif

}

void sub13(struct dt0 *dt) {

#ifdef CMPLX
   dt->a = dt->a + 5.0f+I*5.0f;
#else
   dt->a = dt->a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt->b = dt->b + 10.0+I*10.0;
#else
   dt->b = dt->b + createcomplex(10.0,10.0);
#endif

}

void sub14(struct dt1 *dt) {

#ifdef CMPLX
   dt->a = dt->a + 5.0f+I*5.0f;
#else
   dt->a = dt->a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt->b = dt->b + 10.0+I*10.0;
#else
   dt->b = dt->b + createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dt->d0.a = dt->d0.a + 5.0f+I*5.0f;
#else
   dt->d0.a = dt->d0.a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt->d0.b = dt->d0.b + 10.0+I*10.0;
#else
   dt->d0.b = dt->d0.b + createcomplex(10.0,10.0);
#endif

}

void sub15(struct dt2 *dt) {

#ifdef CMPLX
   dt->a = dt->a + 5.0f+I*5.0f;
#else
   dt->a = dt->a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt->b = dt->b + 10.0+I*10.0;
#else
   dt->b = dt->b + createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dt->d1.a = dt->d1.a + 5.0f+I*5.0f;
#else
   dt->d1.a = dt->d1.a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt->d1.b = dt->d1.b + 10.0+I*10.0;
#else
   dt->d1.b = dt->d1.b + createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dt->d1.d0.a = dt->d1.d0.a + 5.0f+I*5.0f;
#else
   dt->d1.d0.a = dt->d1.d0.a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dt->d1.d0.b = dt->d1.d0.b + 10.0+I*10.0;
#else
   dt->d1.d0.b = dt->d1.d0.b + createcomplex(10.0,10.0);
#endif

}

