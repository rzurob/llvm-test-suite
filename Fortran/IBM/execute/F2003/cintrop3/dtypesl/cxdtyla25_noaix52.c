
#include <stdio.h>
#include <stdlib.h>


  #define int_fast16_t short

#define DIM1 2
#define DIM2 3

typedef struct dt1 DT1;
typedef struct dt2 DT2;
typedef struct dt3 DT3;

struct dt1 {
   _Bool var_a[DIM1][DIM2];
   double var_b[DIM1][DIM2];
   char var_c[DIM1][DIM2];
   float var_d[DIM1][DIM2];
   long double var_e[DIM1][DIM2];
   double var_f[DIM1][DIM2];
   _Bool var_g[DIM1][DIM2];
   float var_h[DIM1][DIM2];
};

struct dt2 {
   double var_a[DIM1][DIM2];
   _Bool var_b[DIM1][DIM2];
   double var_c[DIM1][DIM2];
   char var_d[DIM1][DIM2];
   long double var_e[DIM1][DIM2];
   float var_f[DIM1][DIM2];
   _Bool var_g[DIM1][DIM2];
   float var_h[DIM1][DIM2];
   DT1 vdt1[DIM2][DIM1];
};

struct dt3 {
   DT2 vdt2[DIM2][DIM1];
   float var_a[DIM1][DIM2];
   _Bool var_b[DIM1][DIM2];
   long double var_c[DIM1][DIM2];
   double var_d[DIM1][DIM2];
   float var_e[DIM1][DIM2];
   _Bool var_f[DIM1][DIM2];
   double var_g[DIM1][DIM2];
   char var_h[DIM1][DIM2];
};

#define ARRDT1 {0,0,0,0,0,0},{2.0,3.0,4.0,5.0,6.0,7.0},{'A','A','A','A','A','A'},{4.0f,5.0f,6.0f,7.0f,8.0f,9.0f},{5.0l,6.0l,7.0l,8.0l,9.0l,10.0l},{6.0,7.0,8.0,9.0,10.0,11.0},{0,0,0,0,0,0},{8.0f,9.0f,10.0f,11.0f,12.0f,13.0f}
#define ARR2 {1.0,2.0,3.0,4.0,5.0,6.0},{0,0,0,0,0,0},{3.0,4.0,5.0,6.0,7.0,8.0},{'A','A','A','A','A','A'},{5.0l,6.0l,7.0l,8.0l,9.0l,10.0l},{6.0f,7.0f,8.0f,9.0f,10.0f,11.0f},{0,0,0,0,0,0},{8.0f,9.0f,10.0f,11.0f,12.0f,13.0f}
#define ARR3 {1.0f,2.0f,3.0f,4.0f,5.0f,6.0f},{0,0,0,0,0,0},{3.0l,4.0l,5.0l,6.0l,7.0l,8.0l},{4.0,5.0,6.0,7.0,8.0,9.0},{5.0f,6.0f,7.0f,8.0f,9.0f,10.0f},{0,0,0,0,0,0},{7.0,8.0,9.0,10.0,11.0,12.0},{'A','A','A','A','A','A'}

#define ARRDT2 ARR2,{{{ARRDT1},{ARRDT1}},{{ARRDT1},{ARRDT1}},{{ARRDT1},{ARRDT1}}}

#define ARRDT3 {{{ARRDT2},{ARRDT2}},{{ARRDT2},{ARRDT2}},{{ARRDT2},{ARRDT2}}},ARR3

DT1 *st1sum(DT1 *, DT1 *);
DT2 *st2sum(DT2 *, DT2 *);
DT3 *st3sum(DT3 *, DT3 *);

int main() {

   DT3 *fun1(DT3 *);
   DT3 *fun2(DT3 *, DT3 *);

   DT3 dt0 = { ARRDT3 };

   DT3 dta[DIM2][DIM1], dtb[DIM2][DIM1];
   DT3 *dtp, *dtr;
   int i, j, k, l;


   for ( i = 0; i < DIM2; i++ ) {
     for ( j = 0; j < DIM1; j++ ) {
       dta[i][j] = dt0;
     }
   }

   dtr = fun1(&dta[0][0]);

   for ( k = 0; k < DIM2; k++ ) {
    for ( l = 0; l < DIM1; l++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
       if ( dta[k][l].var_a[i][j] != (float)(i*DIM2+j+2) ) exit(21);
       if ( dta[k][l].var_b[i][j] != 1 ) exit(23);
       if ( dta[k][l].var_c[i][j] != (long double)(i*DIM2+j+4) ) exit(25);
       if ( dta[k][l].var_d[i][j] != (double)(i*DIM2+j+5) ) exit(27);
       if ( dta[k][l].var_e[i][j] != (float)(i*DIM2+j+6) ) exit(29);
       if ( dta[k][l].var_f[i][j] != 1 ) exit(31);
       if ( dta[k][l].var_g[i][j] != (double)(i*DIM2+j+8) ) exit(33);
       if ( dta[k][l].var_h[i][j] != 'B' ) exit(35);

       if ( (dtr+k*DIM1+l)->var_a[i][j] != (float)(i*DIM2+j+2) ) exit(37);
       if ( (dtr+k*DIM1+l)->var_b[i][j] != 1 ) exit(39);
       if ( (dtr+k*DIM1+l)->var_c[i][j] != (long double)(i*DIM2+j+4) ) exit(41);
       if ( (dtr+k*DIM1+l)->var_d[i][j] != (double)(i*DIM2+j+5) ) exit(43);
       if ( (dtr+k*DIM1+l)->var_e[i][j] != (float)(i*DIM2+j+6) ) exit(45);
       if ( (dtr+k*DIM1+l)->var_f[i][j] != 1 ) exit(47);
       if ( (dtr+k*DIM1+l)->var_g[i][j] != (double)(i*DIM2+j+8) ) exit(49);
       if ( (dtr+k*DIM1+l)->var_h[i][j] != 'B' ) exit(51);

       if ( dta[k][l].vdt2[j][i].var_a[i][j] != (double)(i*DIM2+j+2) ) exit(53);
       if ( dta[k][l].vdt2[j][i].var_b[i][j] != 1 ) exit(55);
       if ( dta[k][l].vdt2[j][i].var_c[i][j] != (double)(i*DIM2+j+4) ) exit(57);
       if ( dta[k][l].vdt2[j][i].var_d[i][j] != 'B' ) exit(59);
       if ( dta[k][l].vdt2[j][i].var_e[i][j] != (long double)(i*DIM2+j+6) ) exit(61);
       if ( dta[k][l].vdt2[j][i].var_f[i][j] != (float)(i*DIM2+j+7) ) exit(63);
       if ( dta[k][l].vdt2[j][i].var_g[i][j] != 1 ) exit(65);
       if ( dta[k][l].vdt2[j][i].var_h[i][j] != (float)(i*DIM2+j+9) ) exit(67);

       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_a[i][j] != (double)(i*DIM2+j+2) ) exit(69);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_b[i][j] != 1 ) exit(71);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_c[i][j] != (double)(i*DIM2+j+4) ) exit(73);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_d[i][j] != 'B' ) exit(75);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_e[i][j] != (long double)(i*DIM2+j+6) ) exit(77);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_f[i][j] != (float)(i*DIM2+j+7) ) exit(79);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_g[i][j] != 1 ) exit(81);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_h[i][j] != (float)(i*DIM2+j+9) ) exit(83);

       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_a[i][j] != 1 ) exit(85);
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_b[i][j] != (double)(i*DIM2+j+3) ) exit(87);
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_c[i][j] != 'B' ) exit(89);
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_d[i][j] != (float)(i*DIM2+j+5) ) exit(91);
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_e[i][j] != (long double)(i*DIM2+j+6) ) exit(93);
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_f[i][j] != (double)(i*DIM2+j+7) ) exit(95);
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_g[i][j] != 1 ) exit(97);
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_h[i][j] != (float)(i*DIM2+j+9) ) exit(99);

       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_a[i][j] != 1 ) exit(101);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_b[i][j] != (double)(i*DIM2+j+3) ) exit(103);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_c[i][j] != 'B' ) exit(105);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_d[i][j] != (float)(i*DIM2+j+5) ) exit(107);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_e[i][j] != (long double)(i*DIM2+j+6) ) exit(109);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_f[i][j] != (double)(i*DIM2+j+7) ) exit(111);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_g[i][j] != 1 ) exit(113);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_h[i][j] != (float)(i*DIM2+j+9) ) exit(115);
      }
     }
    }
   }


   for ( i = 0; i < DIM2; i++ ) {
     for ( j = 0; j < DIM1; j++ ) {
       dta[i][j] = dt0;
       dtb[i][j] = dt0;
     }
   }

   dtp = malloc(sizeof(DT3)*DIM1*DIM2);
   dtp = st3sum(&dta[0][0],&dta[0][0]);

   dtr = fun2(dtp,&dtb[0][0]);

   for ( k = 0; k < DIM2; k++ ) {
    for ( l = 0; l < DIM1; l++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
       if ( dtb[k][l].var_a[i][j] != (float)(2*(i*DIM2+j+2)-1) ) exit(117);
       if ( dtb[k][l].var_b[i][j] != 1 ) exit(119);
       if ( dtb[k][l].var_c[i][j] != (long double)(2*(i*DIM2+j+4)+1) ) exit(121);
       if ( dtb[k][l].var_d[i][j] != (double)(2*(i*DIM2+j+5)+2) ) exit(123);
       if ( dtb[k][l].var_e[i][j] != (float)(2*(i*DIM2+j+6)+3) ) exit(125);
       if ( dtb[k][l].var_f[i][j] != 1 ) exit(127);
       if ( dtb[k][l].var_g[i][j] != (double)(2*(i*DIM2+j+8)+5) ) exit(129);
       if ( dtb[k][l].var_h[i][j] != 'B' ) exit(131);

       if ( (dtr+k*DIM1+l)->var_a[i][j] != (float)(2*(i*DIM2+j+2)-1) ) exit(133);
       if ( (dtr+k*DIM1+l)->var_b[i][j] != 1 ) exit(135);
       if ( (dtr+k*DIM1+l)->var_c[i][j] != (long double)(2*(i*DIM2+j+4)+1) ) exit(137);
       if ( (dtr+k*DIM1+l)->var_d[i][j] != (double)(2*(i*DIM2+j+5)+2) ) exit(139);
       if ( (dtr+k*DIM1+l)->var_e[i][j] != (float)(2*(i*DIM2+j+6)+3) ) exit(141);
       if ( (dtr+k*DIM1+l)->var_f[i][j] != 1 ) exit(143);
       if ( (dtr+k*DIM1+l)->var_g[i][j] != (double)(2*(i*DIM2+j+8)+5) ) exit(145);
       if ( (dtr+k*DIM1+l)->var_h[i][j] != 'B' ) exit(147);

       if ( dtb[k][l].vdt2[j][i].var_a[i][j] != (double)(2*(i*DIM2+j+2)-1) ) exit(149);
       if ( dtb[k][l].vdt2[j][i].var_b[i][j] != 1 ) exit(151);
       if ( dtb[k][l].vdt2[j][i].var_c[i][j] != (double)(2*(i*DIM2+j+4)+1) ) exit(153);
       if ( dtb[k][l].vdt2[j][i].var_d[i][j] != 'B' ) exit(155);
       if ( dtb[k][l].vdt2[j][i].var_e[i][j] != (long double)(2*(i*DIM2+j+6)+3) ) exit(157);
       if ( dtb[k][l].vdt2[j][i].var_f[i][j] != (float)(2*(i*DIM2+j+7)+4) ) exit(159);
       if ( dtb[k][l].vdt2[j][i].var_g[i][j] != 1 ) exit(161);
       if ( dtb[k][l].vdt2[j][i].var_h[i][j] != (float)(2*(i*DIM2+j+9)+6) ) exit(163);

       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_a[i][j] != (double)(2*(i*DIM2+j+2)-1) ) exit(165);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_b[i][j] != 1 ) exit(167);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_c[i][j] != (double)(2*(i*DIM2+j+4)+1) ) exit(169);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_d[i][j] != 'B' ) exit(171);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_e[i][j] != (long double)(2*(i*DIM2+j+6)+3) ) exit(173);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_f[i][j] != (float)(2*(i*DIM2+j+7)+4) ) exit(175);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_g[i][j] != 1 ) exit(177);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_h[i][j] != (float)(2*(i*DIM2+j+9)+6) ) exit(179);

       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_a[i][j] != 1 ) exit(181);
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_b[i][j] != (double)(2*(i*DIM2+j+3)) ) exit(183);
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_c[i][j] != 'B' ) exit(185);
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_d[i][j] != (float)(2*(i*DIM2+j+5)+2) ) exit(187);
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_e[i][j] != (long double)(2*(i*DIM2+j+6)+3) ) exit(189);
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_f[i][j] != (double)(2*(i*DIM2+j+7)+4) ) exit(191);
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_g[i][j] != 1 ) exit(193);
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_h[i][j] != (float)(2*(i*DIM2+j+9)+6) ) exit(195);

       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_a[i][j] != 1 ) exit(197);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_b[i][j] != (double)(2*(i*DIM2+j+3)) ) exit(199);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_c[i][j] != 'B' ) exit(201);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_d[i][j] != (float)(2*(i*DIM2+j+5)+2) ) exit(203);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_e[i][j] != (long double)(2*(i*DIM2+j+6)+3) ) exit(205);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_f[i][j] != (double)(2*(i*DIM2+j+7)+4) ) exit(207);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_g[i][j] != 1 ) exit(209);
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_h[i][j] != (float)(2*(i*DIM2+j+9)+6) ) exit(211);
      }
     }
    }
   }

   return 0;
}

DT1 *st1sum(DT1 *dtx, DT1 *dty) {
   DT1 *dtp;
   int i, j, k;

   dtp = malloc(sizeof(DT1));

   for ( i = 0; i < DIM1; i++ ) {
    for ( j = 0; j < DIM2; j++ ) {
     dtp->var_a[i][j] = dtx->var_a[i][j] && dty->var_a[i][j];
     dtp->var_b[i][j] = dtx->var_b[i][j] + dty->var_b[i][j];
     dtp->var_c[i][j] = dtx->var_c[i][j];
     dtp->var_d[i][j] = dtx->var_d[i][j] + dty->var_d[i][j];
     dtp->var_e[i][j] = dtx->var_e[i][j] + dty->var_e[i][j];
     dtp->var_f[i][j] = dtx->var_f[i][j] + dty->var_f[i][j];
     dtp->var_g[i][j] = dtx->var_g[i][j] && dty->var_g[i][j];
     dtp->var_h[i][j] = dtx->var_h[i][j] + dty->var_h[i][j];
    }
   }

   return(dtp);
}

DT2 *st2sum(DT2 *dtx, DT2 *dty) {
   DT2 *dtp;
   DT1 *dtq;
   int i, j, k;

   dtp = malloc(sizeof(DT2));

   for ( i = 0; i < DIM1; i++ ) {
    for ( j = 0; j < DIM2; j++ ) {
     dtp->var_a[i][j] = dtx->var_a[i][j] + dty->var_a[i][j];
     dtp->var_b[i][j] = dtx->var_b[i][j] && dty->var_b[i][j];
     dtp->var_c[i][j] = dtx->var_c[i][j] + dty->var_c[i][j];
     dtp->var_d[i][j] = dtx->var_d[i][j];
     dtp->var_e[i][j] = dtx->var_e[i][j] + dty->var_e[i][j];
     dtp->var_f[i][j] = dtx->var_f[i][j] + dty->var_f[i][j];
     dtp->var_g[i][j] = dtx->var_g[i][j] && dty->var_g[i][j];
     dtp->var_h[i][j] = dtx->var_h[i][j] + dty->var_h[i][j];

     dtq = st1sum(&dtx->vdt1[j][i],&dty->vdt1[j][i]);
     dtp->vdt1[j][i] = *dtq;
    }
   }

   return(dtp);
}

DT3 *st3sum(DT3 *dtx, DT3 *dty) {
   DT3 *dtp;
   DT2 *dtq;
   int i, j, k;

   dtp = malloc(sizeof(DT3)*DIM1*DIM2);

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
      (dtp+k)->var_a[i][j] = (dtx+k)->var_a[i][j] + (dty+k)->var_a[i][j];
      (dtp+k)->var_b[i][j] = (dtx+k)->var_b[i][j] && (dty+k)->var_b[i][j];
      (dtp+k)->var_c[i][j] = (dtx+k)->var_c[i][j] + (dty+k)->var_c[i][j];
      (dtp+k)->var_d[i][j] = (dtx+k)->var_d[i][j] + (dty+k)->var_d[i][j];
      (dtp+k)->var_e[i][j] = (dtx+k)->var_e[i][j] + (dty+k)->var_e[i][j];
      (dtp+k)->var_f[i][j] = (dtx+k)->var_f[i][j] && (dty+k)->var_f[i][j];
      (dtp+k)->var_g[i][j] = (dtx+k)->var_g[i][j] + (dty+k)->var_g[i][j];
      (dtp+k)->var_h[i][j] = (dtx+k)->var_h[i][j];

      dtq = st2sum(&(dtx+k)->vdt2[j][i],&(dty+k)->vdt2[j][i]);
      (dtp+k)->vdt2[j][i] = *dtq;
     }
    }
   }

   return(dtp);
}
