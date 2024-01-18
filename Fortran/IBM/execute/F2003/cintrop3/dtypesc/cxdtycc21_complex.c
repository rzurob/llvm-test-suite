
#include <stdio.h>
#include <stdlib.h>

   #include <complex.h>

#include <inttypes.h>
#include <stddef.h>

typedef struct dt1 DT1;
typedef struct dt2 DT2;
typedef struct dt3 DT3;

#define DIM1 2
#define DIM2 3

struct dt1 {
   int_fast16_t var_a[DIM1][DIM2];
   double _Complex var_b[DIM1][DIM2];
   signed char var_c[DIM1][DIM2];
   float _Complex var_d[DIM1][DIM2];
   long double _Complex var_e[DIM1][DIM2];
   double _Complex var_f[DIM1][DIM2];
   intmax_t var_g[DIM1][DIM2];
   float _Complex var_h[DIM1][DIM2];
};

struct dt2 {
   double _Complex var_a[DIM1][DIM2];
   int var_b[DIM1][DIM2];
   double _Complex var_c[DIM1][DIM2];
   char var_d[DIM1][DIM2];
   long double _Complex var_e[DIM1][DIM2];
   float _Complex var_f[DIM1][DIM2];
   int32_t var_g[DIM1][DIM2];
   float _Complex var_h[DIM1][DIM2];
   DT1 vdt1[DIM2][DIM1];
};

struct dt3 {
   DT2 vdt2[DIM2][DIM1];
   float _Complex var_a[DIM1][DIM2];
   short var_b[DIM1][DIM2];
   long double _Complex var_c[DIM1][DIM2];
   double _Complex var_d[DIM1][DIM2];
   float _Complex var_e[DIM1][DIM2];
   char var_f[DIM1][DIM2];
   double _Complex var_g[DIM1][DIM2];
   int var_h[DIM1][DIM2];
};

DT3 *fun1(DT3 *dt) {
   int i, j, k;

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
       if ( (dt+k)->var_a[i][j] != (float)(i*DIM2+j+1)+I*(float)(i*DIM2+j+1) ) exit(21);
       if ( (dt+k)->var_b[i][j] != i*DIM2+j+2 ) exit(23);
       if ( (dt+k)->var_c[i][j] != (long double)(i*DIM2+j+3)+I*(long double)(i*DIM2+j+3) ) exit(25);
       if ( (dt+k)->var_d[i][j] != (double)(i*DIM2+j+4)+I*(double)(i*DIM2+j+4) ) exit(27);
       if ( (dt+k)->var_e[i][j] != (float)(i*DIM2+j+5)+I*(float)(i*DIM2+j+5) ) exit(29);
       if ( (dt+k)->var_f[i][j] != i*DIM2+j+6 ) exit(31);
       if ( (dt+k)->var_g[i][j] != (double)(i*DIM2+j+7)+I*(double)(i*DIM2+j+7) ) exit(33);
       if ( (dt+k)->var_h[i][j] != i*DIM2+j+8 ) exit(35);

       if ( (dt+k)->vdt2[j][i].var_a[i][j] != (double)(i*DIM2+j+1)+I*(double)(i*DIM2+j+1) ) exit(37);
       if ( (dt+k)->vdt2[j][i].var_b[i][j] != i*DIM2+j+2 ) exit(39);
       if ( (dt+k)->vdt2[j][i].var_c[i][j] != (double)(i*DIM2+j+3)+I*(double)(i*DIM2+j+3) ) exit(41);
       if ( (dt+k)->vdt2[j][i].var_d[i][j] != i*DIM2+j+4 ) exit(43);
       if ( (dt+k)->vdt2[j][i].var_e[i][j] != (long double)(i*DIM2+j+5)+I*(long double)(i*DIM2+j+5) ) exit(45);
       if ( (dt+k)->vdt2[j][i].var_f[i][j] != (float)(i*DIM2+j+6)+I*(float)(i*DIM2+j+6) ) exit(47);
       if ( (dt+k)->vdt2[j][i].var_g[i][j] != i*DIM2+j+7 ) exit(49);
       if ( (dt+k)->vdt2[j][i].var_h[i][j] != (float)(i*DIM2+j+8)+I*(float)(i*DIM2+j+8) ) exit(51);

       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_a[i][j] != i*DIM2+j+1 ) exit(53);
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_b[i][j] != (double)(i*DIM2+j+2)+I*(double)(i*DIM2+j+2) ) exit(55);
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_c[i][j] != i*DIM2+j+3 ) exit(57);
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_d[i][j] != (float)(i*DIM2+j+4)+I*(float)(i*DIM2+j+4) ) exit(59);
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_e[i][j] != (long double)(i*DIM2+j+5)+I*(long double)(i*DIM2+j+5) ) exit(61);
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_f[i][j] != (double)(i*DIM2+j+6)+I*(double)(i*DIM2+j+6) ) exit(63);
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_g[i][j] != i*DIM2+j+7 ) exit(65);
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_h[i][j] != (float)(i*DIM2+j+8)+I*(float)(i*DIM2+j+8) ) exit(67);
     }
    }
   }

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
       (dt+k)->var_a[i][j] += 1.0f+I*1.0f;
       (dt+k)->var_b[i][j] += 1;
       (dt+k)->var_c[i][j] += 1.0l+I*1.0l;
       (dt+k)->var_d[i][j] += 1.0+I*1.0;
       (dt+k)->var_e[i][j] += 1.0f+I*1.0f;
       (dt+k)->var_f[i][j] += 1;
       (dt+k)->var_g[i][j] += 1.0+I*1.0;
       (dt+k)->var_h[i][j] += 1;

       (dt+k)->vdt2[j][i].var_a[i][j] += 2.0+I*2.0;
       (dt+k)->vdt2[j][i].var_b[i][j] += 2;
       (dt+k)->vdt2[j][i].var_c[i][j] += 2.0+I*2.0;
       (dt+k)->vdt2[j][i].var_d[i][j] += 2;
       (dt+k)->vdt2[j][i].var_e[i][j] += 2.0l+I*2.0l;
       (dt+k)->vdt2[j][i].var_f[i][j] += 2.0f+I*2.0f;
       (dt+k)->vdt2[j][i].var_g[i][j] += 2;
       (dt+k)->vdt2[j][i].var_h[i][j] += 2.0f+I*2.0f;

       (dt+k)->vdt2[j][i].vdt1[j][i].var_a[i][j] += 3;
       (dt+k)->vdt2[j][i].vdt1[j][i].var_b[i][j] += 3.0+I*3.0;
       (dt+k)->vdt2[j][i].vdt1[j][i].var_c[i][j] += 3;
       (dt+k)->vdt2[j][i].vdt1[j][i].var_d[i][j] += 3.0f+I*3.0f;
       (dt+k)->vdt2[j][i].vdt1[j][i].var_e[i][j] += 3.0l+I*3.0l;
       (dt+k)->vdt2[j][i].vdt1[j][i].var_f[i][j] += 3.0+I*3.0;
       (dt+k)->vdt2[j][i].vdt1[j][i].var_g[i][j] += 3;
       (dt+k)->vdt2[j][i].vdt1[j][i].var_h[i][j] += 3.0f+I*3.0f;
     }
    }
   }
   return(dt);
}

DT3 *fun2(DT3 *dtx, DT3 *dty) {
   int i, j, k;

   DT3 *dtz;

   dtz = malloc(sizeof(DT3)*DIM1*DIM2);

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
       if ( (dtx+k)->var_a[i][j] != (float)(2*(i*DIM2+j+1))+I*(float)(2*(i*DIM2+j+1)) ) exit(69);
       if ( (dtx+k)->var_b[i][j] != 2*(i*DIM2+j+2) ) exit(71);
       if ( (dtx+k)->var_c[i][j] != (long double)(2*(i*DIM2+j+3))+I*(long double)(2*(i*DIM2+j+3)) ) exit(73);
       if ( (dtx+k)->var_d[i][j] != (double)(2*(i*DIM2+j+4))+I*(double)(2*(i*DIM2+j+4)) ) exit(75);
       if ( (dtx+k)->var_e[i][j] != (float)(2*(i*DIM2+j+5))+I*(float)(2*(i*DIM2+j+5)) ) exit(77);
       if ( (dtx+k)->var_f[i][j] != 2*(i*DIM2+j+6) ) exit(79);
       if ( (dtx+k)->var_g[i][j] != (double)(2*(i*DIM2+j+7))+I*(double)(2*(i*DIM2+j+7)) ) exit(81);
       if ( (dtx+k)->var_h[i][j] != 2*(i*DIM2+j+8) ) exit(83);

       if ( (dtx+k)->vdt2[j][i].var_a[i][j] != (double)(2*(i*DIM2+j+1))+I*(double)(2*(i*DIM2+j+1)) ) exit(85);
       if ( (dtx+k)->vdt2[j][i].var_b[i][j] != 2*(i*DIM2+j+2) ) exit(87);
       if ( (dtx+k)->vdt2[j][i].var_c[i][j] != (double)(2*(i*DIM2+j+3))+I*(double)(2*(i*DIM2+j+3)) ) exit(89);
       if ( (dtx+k)->vdt2[j][i].var_d[i][j] != 2*(i*DIM2+j+4) ) exit(91);
       if ( (dtx+k)->vdt2[j][i].var_e[i][j] != (long double)(2*(i*DIM2+j+5))+I*(long double)(2*(i*DIM2+j+5)) ) exit(93);
       if ( (dtx+k)->vdt2[j][i].var_f[i][j] != (float)(2*(i*DIM2+j+6))+I*(float)(2*(i*DIM2+j+6)) ) exit(95);
       if ( (dtx+k)->vdt2[j][i].var_g[i][j] != 2*(i*DIM2+j+7) ) exit(97);
       if ( (dtx+k)->vdt2[j][i].var_h[i][j] != (float)(2*(i*DIM2+j+8))+I*(float)(2*(i*DIM2+j+8)) ) exit(99);

       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_a[i][j] != 2*(i*DIM2+j+1) ) exit(101);
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_b[i][j] != (double)(2*(i*DIM2+j+2))+I*(double)(2*(i*DIM2+j+2)) ) exit(103);
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_c[i][j] != 2*(i*DIM2+j+3) ) exit(105);
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_d[i][j] != (float)(2*(i*DIM2+j+4))+I*(float)(2*(i*DIM2+j+4)) ) exit(107);
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_e[i][j] != (long double)(2*(i*DIM2+j+5))+I*(long double)(2*(i*DIM2+j+5)) ) exit(109);
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_f[i][j] != (double)(2*(i*DIM2+j+6))+I*(double)(2*(i*DIM2+j+6)) ) exit(111);
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_g[i][j] != 2*(i*DIM2+j+7) ) exit(113);
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_h[i][j] != (float)(2*(i*DIM2+j+8))+I*(float)(2*(i*DIM2+j+8)) ) exit(115);
     }
    }
   }

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
       (dty+k)->var_a[i][j] = (dtx+k)->var_a[i][j] + 1.0f+I*1.0f;
       (dty+k)->var_b[i][j] = (dtx+k)->var_b[i][j] + 1;
       (dty+k)->var_c[i][j] = (dtx+k)->var_c[i][j] + 1.0l+I*1.0l;
       (dty+k)->var_d[i][j] = (dtx+k)->var_d[i][j] + 1.0+I*1.0;
       (dty+k)->var_e[i][j] = (dtx+k)->var_e[i][j] + 1.0f+I*1.0f;
       (dty+k)->var_f[i][j] = (dtx+k)->var_f[i][j] + 1;
       (dty+k)->var_g[i][j] = (dtx+k)->var_g[i][j] + 1.0+I*1.0;
       (dty+k)->var_h[i][j] = (dtx+k)->var_h[i][j] + 1;

       (dty+k)->vdt2[j][i].var_a[i][j] = (dtx+k)->vdt2[j][i].var_a[i][j] + 2.0+I*2.0;
       (dty+k)->vdt2[j][i].var_b[i][j] = (dtx+k)->vdt2[j][i].var_b[i][j] + 2;
       (dty+k)->vdt2[j][i].var_c[i][j] = (dtx+k)->vdt2[j][i].var_c[i][j] + 2.0+I*2.0;
       (dty+k)->vdt2[j][i].var_d[i][j] = (dtx+k)->vdt2[j][i].var_d[i][j] + 2;
       (dty+k)->vdt2[j][i].var_e[i][j] = (dtx+k)->vdt2[j][i].var_e[i][j] + 2.0l+I*2.0l;
       (dty+k)->vdt2[j][i].var_f[i][j] = (dtx+k)->vdt2[j][i].var_f[i][j] + 2.0f+I*2.0f;
       (dty+k)->vdt2[j][i].var_g[i][j] = (dtx+k)->vdt2[j][i].var_g[i][j] + 2;
       (dty+k)->vdt2[j][i].var_h[i][j] = (dtx+k)->vdt2[j][i].var_h[i][j] + 2.0f+I*2.0f;

       (dty+k)->vdt2[j][i].vdt1[j][i].var_a[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_a[i][j] + 3;
       (dty+k)->vdt2[j][i].vdt1[j][i].var_b[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_b[i][j] + 3.0+I*3.0;
       (dty+k)->vdt2[j][i].vdt1[j][i].var_c[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_c[i][j] + 3;
       (dty+k)->vdt2[j][i].vdt1[j][i].var_d[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_d[i][j] + 3.0f+I*3.0f;
       (dty+k)->vdt2[j][i].vdt1[j][i].var_e[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_e[i][j] + 3.0l+I*3.0l;
       (dty+k)->vdt2[j][i].vdt1[j][i].var_f[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_f[i][j] + 3.0+I*3.0;
       (dty+k)->vdt2[j][i].vdt1[j][i].var_g[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_g[i][j] + 3;
       (dty+k)->vdt2[j][i].vdt1[j][i].var_h[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_h[i][j] + 3.0f+I*3.0f;
     }
    }
   }
   dtz = dty;
   return(dtz);
}

