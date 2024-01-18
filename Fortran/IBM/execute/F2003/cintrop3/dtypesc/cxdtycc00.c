
#include <stdio.h>
#include <stdlib.h>

#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

typedef struct dt1 DT1;

struct dt1 {
   float _Complex var_a;
   short var_b;
   long double _Complex var_c;
   double _Complex var_d;
   float _Complex var_e;
   char var_f;
   double _Complex var_g;
   int var_h;
};

void sub1(DT1 *dt) {

#ifdef CMPLX
   if ( dt->var_a != 2.0f+I*2.0f ) exit(21);
#else
   if ( dt->var_a != createcomplexf(2.0f,2.0f) ) exit(21);
#endif
   if ( dt->var_b != 4 ) exit(23);
#ifdef CMPLX
   if ( dt->var_c != 6.0l+I*6.0l ) exit(25);
#else
   if ( dt->var_c != createcomplexl(6.0l,6.0l) ) exit(25);
#endif
#ifdef CMPLX
   if ( dt->var_d != 8.0+I*8.0 ) exit(27);
#else
   if ( dt->var_d != createcomplex(8.0,8.0) ) exit(27);
#endif
#ifdef CMPLX
   if ( dt->var_e != 10.0f+I*10.0f ) exit(29);
#else
   if ( dt->var_e != createcomplexf(10.0f,10.0f) ) exit(29);
#endif
   if ( dt->var_f != 12 ) exit(31);
#ifdef CMPLX
   if ( dt->var_g != 14.0+I*14.0 ) exit(33);
#else
   if ( dt->var_g != createcomplex(14.0,14.0) ) exit(33);
#endif
   if ( dt->var_h != 16 ) exit(35);

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
}

void sub2(DT1 dt) {

#ifdef CMPLX
   if ( dt.var_a != 2.0f+I*2.0f ) exit(37);
#else
   if ( dt.var_a != createcomplexf(2.0f,2.0f) ) exit(37);
#endif
   if ( dt.var_b != 4 ) exit(39);
#ifdef CMPLX
   if ( dt.var_c != 6.0l+I*6.0l ) exit(41);
#else
   if ( dt.var_c != createcomplexl(6.0l,6.0l) ) exit(41);
#endif
#ifdef CMPLX
   if ( dt.var_d != 8.0+I*8.0 ) exit(43);
#else
   if ( dt.var_d != createcomplex(8.0,8.0) ) exit(43);
#endif
#ifdef CMPLX
   if ( dt.var_e != 10.0f+I*10.0f ) exit(45);
#else
   if ( dt.var_e != createcomplexf(10.0f,10.0f) ) exit(45);
#endif
   if ( dt.var_f != 12 ) exit(47);
#ifdef CMPLX
   if ( dt.var_g != 14.0+I*14.0 ) exit(49);
#else
   if ( dt.var_g != createcomplex(14.0,14.0) ) exit(49);
#endif
   if ( dt.var_h != 16 ) exit(51);

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
}

void sub3(DT1 *dtx, DT1 *dty) {

#ifdef CMPLX
   if ( dtx->var_a != 4.0f+I*4.0f ) exit(53);
#else
   if ( dtx->var_a != createcomplexf(4.0f,4.0f) ) exit(53);
#endif
   if ( dtx->var_b != 8 ) exit(55);
#ifdef CMPLX
   if ( dtx->var_c != 12.0l+I*12.0l ) exit(57);
#else
   if ( dtx->var_c != createcomplexl(12.0l,12.0l) ) exit(57);
#endif
#ifdef CMPLX
   if ( dtx->var_d != 16.0+I*16.0 ) exit(59);
#else
   if ( dtx->var_d != createcomplex(16.0,16.0) ) exit(59);
#endif
#ifdef CMPLX
   if ( dtx->var_e != 20.0f+I*20.0f ) exit(61);
#else
   if ( dtx->var_e != createcomplexf(20.0f,20.0f) ) exit(61);
#endif
   if ( dtx->var_f != 24 ) exit(63);
#ifdef CMPLX
   if ( dtx->var_g != 28.0+I*28.0 ) exit(65);
#else
   if ( dtx->var_g != createcomplex(28.0,28.0) ) exit(65);
#endif
   if ( dtx->var_h != 32 ) exit(67);

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
   
}
