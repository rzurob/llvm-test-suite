
#include <stdio.h>
#include <stdlib.h>

#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

#include <inttypes.h>
#include <stddef.h>

#if ( defined(_AIX) && ! defined(_AIX52) )
  #define int_fast16_t short
#endif

typedef struct dt1 DT1;
typedef struct dt2 DT2;

struct dt1 {
   int_fast16_t var_a;
   double _Complex var_b;
   signed char var_c;
   float _Complex var_d;
   long double _Complex var_e;
   double _Complex var_f;
   intmax_t var_g;
   float _Complex var_h;
};

struct dt2 {
   float _Complex var_a;
   short var_b;
   long double _Complex var_c;
   double _Complex var_d;
   float _Complex var_e;
   char var_f;
   double _Complex var_g;
   int var_h;
   DT1 vdt1;
};

void sub1(DT2 *dt) {

#ifdef CMPLX
   if ( dt->var_a != 2.0f+I*2.0f || dt->vdt1.var_a != 2 ) exit(21);
#else
   if ( dt->var_a != createcomplexf(2.0f,2.0f) || dt->vdt1.var_a != 2 ) exit(21);
#endif
#ifdef CMPLX
   if ( dt->var_b != 4 || dt->vdt1.var_b != 4.0+I*4.0 ) exit(23);
#else
   if ( dt->var_b != 4 || dt->vdt1.var_b != createcomplex(4.0,4.0) ) exit(23);
#endif
#ifdef CMPLX
   if ( dt->var_c != 6.0l+I*6.0l || dt->vdt1.var_c != 6 ) exit(25);
#else
   if ( dt->var_c != createcomplexl(6.0l,6.0l) || dt->vdt1.var_c != 6 ) exit(25);
#endif
#ifdef CMPLX
   if ( dt->var_d != 8.0+I*8.0 || dt->vdt1.var_d != 8.0f+I*8.0f ) exit(27);
#else
   if ( dt->var_d != createcomplex(8.0,8.0) || dt->vdt1.var_d != createcomplexf(8.0f,8.0f) ) exit(27);
#endif
#ifdef CMPLX
   if ( dt->var_e != 10.0f+I*10.0f || dt->vdt1.var_e != 10.0l+I*10.0l ) exit(29);
#else
   if ( dt->var_e != createcomplexf(10.0f,10.0f) || dt->vdt1.var_e != createcomplexl(10.0l,10.0l) ) exit(29);
#endif
#ifdef CMPLX
   if ( dt->var_f != 12 || dt->vdt1.var_f != 12.0+I*12.0 ) exit(31);
#else
   if ( dt->var_f != 12 || dt->vdt1.var_f != createcomplex(12.0,12.0) ) exit(31);
#endif
#ifdef CMPLX
   if ( dt->var_g != 14.0+I*14.0 || dt->vdt1.var_g != 14 ) exit(33);
#else
   if ( dt->var_g != createcomplex(14.0,14.0) || dt->vdt1.var_g != 14 ) exit(33);
#endif
#ifdef CMPLX
   if ( dt->var_h != 16 || dt->vdt1.var_h != 16.0f+I*16.0f ) exit(35);
#else
   if ( dt->var_h != 16 || dt->vdt1.var_h != createcomplexf(16.0f,16.0f) ) exit(35);
#endif

#ifdef CMPLX
   dt->var_a += 1.0f+I*1.0f;
#else
   dt->var_a += createcomplexf(1.0f,1.0f);
#endif
   dt->var_b += 1;
#ifdef CMPLX
   dt->var_c += 1.0l+I*1.0l;
#else
   dt->var_c += createcomplexl(1.0l,1.0l);
#endif
#ifdef CMPLX
   dt->var_d += 1.0+I*1.0;
#else
   dt->var_d += createcomplex(1.0,1.0);
#endif
#ifdef CMPLX
   dt->var_e += 1.0f+I*1.0f;
#else
   dt->var_e += createcomplexf(1.0f,1.0f);
#endif
   dt->var_f += 1;
#ifdef CMPLX
   dt->var_g += 1.0+I*1.0;
#else
   dt->var_g += createcomplex(1.0,1.0);
#endif
   dt->var_h += 1;

   dt->vdt1.var_a += 2;
#ifdef CMPLX
   dt->vdt1.var_b += 2.0+I*2.0;
#else
   dt->vdt1.var_b += createcomplex(2.0,2.0);
#endif
   dt->vdt1.var_c += 2;
#ifdef CMPLX
   dt->vdt1.var_d += 2.0f+I*2.0f;
#else
   dt->vdt1.var_d += createcomplexf(2.0f,2.0f);
#endif
#ifdef CMPLX
   dt->vdt1.var_e += 2.0l+I*2.0l;
#else
   dt->vdt1.var_e += createcomplexl(2.0l,2.0l);
#endif
#ifdef CMPLX
   dt->vdt1.var_f += 2.0+I*2.0;
#else
   dt->vdt1.var_f += createcomplex(2.0,2.0);
#endif
   dt->vdt1.var_g += 2;
#ifdef CMPLX
   dt->vdt1.var_h += 2.0f+I*2.0f;
#else
   dt->vdt1.var_h += createcomplexf(2.0f,2.0f);
#endif

}

void sub2(DT2 dt) {

#ifdef CMPLX
   if ( dt.var_a != 2.0f+I*2.0f || dt.vdt1.var_a != 2 ) exit(37);
#else
   if ( dt.var_a != createcomplexf(2.0f,2.0f) || dt.vdt1.var_a != 2 ) exit(37);
#endif
#ifdef CMPLX
   if ( dt.var_b != 4 || dt.vdt1.var_b != 4.0+I*4.0 ) exit(39);
#else
   if ( dt.var_b != 4 || dt.vdt1.var_b != createcomplex(4.0,4.0) ) exit(39);
#endif
#ifdef CMPLX
   if ( dt.var_c != 6.0l+I*6.0l || dt.vdt1.var_c != 6 ) exit(41);
#else
   if ( dt.var_c != createcomplexl(6.0l,6.0l) || dt.vdt1.var_c != 6 ) exit(41);
#endif
#ifdef CMPLX
   if ( dt.var_d != 8.0+I*8.0 || dt.vdt1.var_d != 8.0f+I*8.0f ) exit(43);
#else
   if ( dt.var_d != createcomplex(8.0,8.0) || dt.vdt1.var_d != createcomplexf(8.0f,8.0f) ) exit(43);
#endif
#ifdef CMPLX
   if ( dt.var_e != 10.0f+I*10.0f || dt.vdt1.var_e != 10.0l+I*10.0l ) exit(45);
#else
   if ( dt.var_e != createcomplexf(10.0f,10.0f) || dt.vdt1.var_e != createcomplexl(10.0l,10.0l) ) exit(45);
#endif
#ifdef CMPLX
   if ( dt.var_f != 12 || dt.vdt1.var_f != 12.0+I*12.0 ) exit(47);
#else
   if ( dt.var_f != 12 || dt.vdt1.var_f != createcomplex(12.0,12.0) ) exit(47);
#endif
#ifdef CMPLX
   if ( dt.var_g != 14.0+I*14.0 || dt.vdt1.var_g != 14 ) exit(49);
#else
   if ( dt.var_g != createcomplex(14.0,14.0) || dt.vdt1.var_g != 14 ) exit(49);
#endif
#ifdef CMPLX
   if ( dt.var_h != 16 || dt.vdt1.var_h != 16.0f+I*16.0f ) exit(51);
#else
   if ( dt.var_h != 16 || dt.vdt1.var_h != createcomplexf(16.0f,16.0f) ) exit(51);
#endif

#ifdef CMPLX
   dt.var_a += 1.0f+I*1.0f;
#else
   dt.var_a += createcomplexf(1.0f,1.0f);
#endif
   dt.var_b += 1;
#ifdef CMPLX
   dt.var_c += 1.0l+I*1.0l;
#else
   dt.var_c += createcomplexl(1.0l,1.0l);
#endif
#ifdef CMPLX
   dt.var_d += 1.0+I*1.0;
#else
   dt.var_d += createcomplex(1.0,1.0);
#endif
#ifdef CMPLX
   dt.var_e += 1.0f+I*1.0f;
#else
   dt.var_e += createcomplexf(1.0f,1.0f);
#endif
   dt.var_f += 1;
#ifdef CMPLX
   dt.var_g += 1.0+I*1.0;
#else
   dt.var_g += createcomplex(1.0,1.0);
#endif
   dt.var_h += 1;

   dt.vdt1.var_a += 2;
#ifdef CMPLX
   dt.vdt1.var_b += 2.0+I*2.0;
#else
   dt.vdt1.var_b += createcomplex(2.0,2.0);
#endif
   dt.vdt1.var_c += 2;
#ifdef CMPLX
   dt.vdt1.var_d += 2.0f+I*2.0f;
#else
   dt.vdt1.var_d += createcomplexf(2.0f,2.0f);
#endif
#ifdef CMPLX
   dt.vdt1.var_e += 2.0l+I*2.0l;
#else
   dt.vdt1.var_e += createcomplexl(2.0l,2.0l);
#endif
#ifdef CMPLX
   dt.vdt1.var_f += 2.0+I*2.0;
#else
   dt.vdt1.var_f += createcomplex(2.0,2.0);
#endif
   dt.vdt1.var_g += 2;
#ifdef CMPLX
   dt.vdt1.var_h += 2.0f+I*2.0f;
#else
   dt.vdt1.var_h += createcomplexf(2.0f,2.0f);
#endif

}

void sub3(DT2 *dtx, DT2 *dty) {

#ifdef CMPLX
   if ( dtx->var_a != 4.0f+I*4.0f || dtx->vdt1.var_a != 4 ) exit(53);
#else
   if ( dtx->var_a != createcomplexf(4.0f,4.0f) || dtx->vdt1.var_a != 4 ) exit(53);
#endif
#ifdef CMPLX
   if ( dtx->var_b != 8 || dtx->vdt1.var_b != 8.0+I*8.0 ) exit(55);
#else
   if ( dtx->var_b != 8 || dtx->vdt1.var_b != createcomplex(8.0,8.0) ) exit(55);
#endif
#ifdef CMPLX
   if ( dtx->var_c != 12.0l+I*12.0l || dtx->vdt1.var_c != 12 ) exit(57);
#else
   if ( dtx->var_c != createcomplexl(12.0l,12.0l) || dtx->vdt1.var_c != 12 ) exit(57);
#endif
#ifdef CMPLX
   if ( dtx->var_d != 16.0+I*16.0 || dtx->vdt1.var_d != 16.0f+I*16.0f ) exit(59);
#else
   if ( dtx->var_d != createcomplex(16.0,16.0) || dtx->vdt1.var_d != createcomplexf(16.0f,16.0f) ) exit(59);
#endif
#ifdef CMPLX
   if ( dtx->var_e != 20.0f+I*20.0f || dtx->vdt1.var_e != 20.0l+I*20.0l ) exit(61);
#else
   if ( dtx->var_e != createcomplexf(20.0f,20.0f) || dtx->vdt1.var_e != createcomplexl(20.0l,20.0l) ) exit(61);
#endif
#ifdef CMPLX
   if ( dtx->var_f != 24 || dtx->vdt1.var_f != 24.0+I*24.0 ) exit(63);
#else
   if ( dtx->var_f != 24 || dtx->vdt1.var_f != createcomplex(24.0,24.0) ) exit(63);
#endif
#ifdef CMPLX
   if ( dtx->var_g != 28.0+I*28.0 || dtx->vdt1.var_g != 28 ) exit(65);
#else
   if ( dtx->var_g != createcomplex(28.0,28.0) || dtx->vdt1.var_g != 28 ) exit(65);
#endif
#ifdef CMPLX
   if ( dtx->var_h != 32 || dtx->vdt1.var_h != 32.0f+I*32.0f ) exit(67);
#else
   if ( dtx->var_h != 32 || dtx->vdt1.var_h != createcomplexf(32.0f,32.0f) ) exit(67);
#endif

#ifdef CMPLX
   dty->var_a = dtx->var_a + 1.0f+I*1.0f;
#else
   dty->var_a = dtx->var_a + createcomplexf(1.0f,1.0f);
#endif
   dty->var_b = dtx->var_b + 1;
#ifdef CMPLX
   dty->var_c = dtx->var_c + 1.0l+I*1.0l;
#else
   dty->var_c = dtx->var_c + createcomplexl(1.0l,1.0l);
#endif
#ifdef CMPLX
   dty->var_d = dtx->var_d + 1.0+I*1.0;
#else
   dty->var_d = dtx->var_d + createcomplex(1.0,1.0);
#endif
#ifdef CMPLX
   dty->var_e = dtx->var_e + 1.0f+I*1.0f;
#else
   dty->var_e = dtx->var_e + createcomplexf(1.0f,1.0f);
#endif
   dty->var_f = dtx->var_f + 1;
#ifdef CMPLX
   dty->var_g = dtx->var_g + 1.0+I*1.0;
#else
   dty->var_g = dtx->var_g + createcomplex(1.0,1.0);
#endif
   dty->var_h = dtx->var_h + 1;

   dty->vdt1.var_a = dtx->vdt1.var_a + 2;
#ifdef CMPLX
   dty->vdt1.var_b = dtx->vdt1.var_b + 2.0+I*2.0;
#else
   dty->vdt1.var_b = dtx->vdt1.var_b + createcomplex(2.0,2.0);
#endif
   dty->vdt1.var_c = dtx->vdt1.var_c + 2;
#ifdef CMPLX
   dty->vdt1.var_d = dtx->vdt1.var_d + 2.0f+I*2.0f;
#else
   dty->vdt1.var_d = dtx->vdt1.var_d + createcomplexf(2.0f,2.0f);
#endif
#ifdef CMPLX
   dty->vdt1.var_e = dtx->vdt1.var_e + 2.0l+I*2.0l;
#else
   dty->vdt1.var_e = dtx->vdt1.var_e + createcomplexl(2.0l,2.0l);
#endif
#ifdef CMPLX
   dty->vdt1.var_f = dtx->vdt1.var_f + 2.0+I*2.0;
#else
   dty->vdt1.var_f = dtx->vdt1.var_f + createcomplex(2.0,2.0);
#endif
   dty->vdt1.var_g = dtx->vdt1.var_g + 2;
#ifdef CMPLX
   dty->vdt1.var_h = dtx->vdt1.var_h + 2.0f+I*2.0f;
#else
   dty->vdt1.var_h = dtx->vdt1.var_h + createcomplexf(2.0f,2.0f);
#endif

}

