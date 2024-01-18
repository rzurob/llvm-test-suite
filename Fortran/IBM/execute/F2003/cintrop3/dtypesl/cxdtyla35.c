
#include <stdio.h>
#include <stdlib.h>


#if ( defined(_AIX) && ! defined(_AIX52) )
  #define int_fast16_t short
#endif

#define DIM1 2
#define DIM2 3

typedef struct dt1 DT1;
typedef struct dt2 DT2;

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
   float var_a[DIM1][DIM2];
   _Bool var_b[DIM1][DIM2];
   long double var_c[DIM1][DIM2];
   double var_d[DIM1][DIM2];
   float var_e[DIM1][DIM2];
   _Bool var_f[DIM1][DIM2];
   double var_g[DIM1][DIM2];
   char var_h[DIM1][DIM2];
   DT1 vdt1[DIM2][DIM1];
};

#define ARRDT1 {0,0,0,0,0,0},{2.0,3.0,4.0,5.0,6.0,7.0},{'A','A','A','A','A','A'},{4.0f,5.0f,6.0f,7.0f,8.0f,9.0f},{5.0l,6.0l,7.0l,8.0l,9.0l,10.0l},{6.0,7.0,8.0,9.0,10.0,11.0},{0,0,0,0,0,0},{8.0f,9.0f,10.0f,11.0f,12.0f,13.0f}
#define ARR2 {1.0f,2.0f,3.0f,4.0f,5.0f,6.0f},{0,0,0,0,0,0},{3.0l,4.0l,5.0l,6.0l,7.0l,8.0l},{4.0,5.0,6.0,7.0,8.0,9.0},{5.0f,6.0f,7.0f,8.0f,9.0f,10.0f},{0,0,0,0,0,0},{7.0,8.0,9.0,10.0,11.0,12.0},{'A','A','A','A','A','A'}

#define ARRDT2 ARR2,{{{ARRDT1},{ARRDT1}},{{ARRDT1},{ARRDT1}},{{ARRDT1},{ARRDT1}}}

DT1 *st1sum(DT1 *, DT1 *);
DT2 *st2sum(DT2 *, DT2 *);

int main() {

   DT2 *fun1(DT2 *);
   DT2 *fun2(DT2 *, DT2 *);

   DT2 dt0 = { ARRDT2 };

   DT2 *dta, *dtb, *dtp, *dtr;
   int i, j, k, l;


   dta = malloc(sizeof(DT2)*DIM2*DIM1);

   for ( k = 0; k < DIM2*DIM1; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
      (dta+k)->var_a[i][j] = dt0.var_a[i][j];
      (dta+k)->var_b[i][j] = dt0.var_b[i][j];
      (dta+k)->var_c[i][j] = dt0.var_c[i][j];
      (dta+k)->var_d[i][j] = dt0.var_d[i][j];
      (dta+k)->var_e[i][j] = dt0.var_e[i][j];
      (dta+k)->var_f[i][j] = dt0.var_f[i][j];
      (dta+k)->var_g[i][j] = dt0.var_g[i][j];
      (dta+k)->var_h[i][j] = dt0.var_h[i][j];

      (dta+k)->vdt1[j][i].var_a[i][j] = dt0.vdt1[j][i].var_a[i][j];
      (dta+k)->vdt1[j][i].var_b[i][j] = dt0.vdt1[j][i].var_b[i][j];
      (dta+k)->vdt1[j][i].var_c[i][j] = dt0.vdt1[j][i].var_c[i][j];
      (dta+k)->vdt1[j][i].var_d[i][j] = dt0.vdt1[j][i].var_d[i][j];
      (dta+k)->vdt1[j][i].var_e[i][j] = dt0.vdt1[j][i].var_e[i][j];
      (dta+k)->vdt1[j][i].var_f[i][j] = dt0.vdt1[j][i].var_f[i][j];
      (dta+k)->vdt1[j][i].var_g[i][j] = dt0.vdt1[j][i].var_g[i][j];
      (dta+k)->vdt1[j][i].var_h[i][j] = dt0.vdt1[j][i].var_h[i][j];
     }
    }
   }

   dtr = fun1(dta);

   for ( k = 0; k < DIM2*DIM1; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
       if ( (dta+k)->var_a[i][j] != (float)(i*DIM2+j+2) ) exit(21);
       if ( (dta+k)->var_b[i][j] != 1 ) exit(23);
       if ( (dta+k)->var_c[i][j] != (long double)(i*DIM2+j+4) ) exit(25);
       if ( (dta+k)->var_d[i][j] != (double)(i*DIM2+j+5) ) exit(27);
       if ( (dta+k)->var_e[i][j] != (float)(i*DIM2+j+6) ) exit(29);
       if ( (dta+k)->var_f[i][j] != 1 ) exit(31);
       if ( (dta+k)->var_g[i][j] != (double)(i*DIM2+j+8) ) exit(33);
       if ( (dta+k)->var_h[i][j] != 'B' ) exit(35);

       if ( (dtr+k)->var_a[i][j] != (float)(i*DIM2+j+2) ) exit(37);
       if ( (dtr+k)->var_b[i][j] != 1 ) exit(39);
       if ( (dtr+k)->var_c[i][j] != (long double)(i*DIM2+j+4) ) exit(41);
       if ( (dtr+k)->var_d[i][j] != (double)(i*DIM2+j+5) ) exit(43);
       if ( (dtr+k)->var_e[i][j] != (float)(i*DIM2+j+6) ) exit(45);
       if ( (dtr+k)->var_f[i][j] != 1 ) exit(47);
       if ( (dtr+k)->var_g[i][j] != (double)(i*DIM2+j+8) ) exit(49);
       if ( (dtr+k)->var_h[i][j] != 'B' ) exit(51);

       if ( (dta+k)->vdt1[j][i].var_a[i][j] != 1 ) exit(53);
       if ( (dta+k)->vdt1[j][i].var_b[i][j] != i*DIM2+j+3 ) exit(55);
       if ( (dta+k)->vdt1[j][i].var_c[i][j] != 'B' ) exit(57);
       if ( (dta+k)->vdt1[j][i].var_d[i][j] != i*DIM2+j+5 ) exit(59);
       if ( (dta+k)->vdt1[j][i].var_e[i][j] != i*DIM2+j+6 ) exit(61);
       if ( (dta+k)->vdt1[j][i].var_f[i][j] != i*DIM2+j+7 ) exit(63);
       if ( (dta+k)->vdt1[j][i].var_g[i][j] != 1 ) exit(65);
       if ( (dta+k)->vdt1[j][i].var_h[i][j] != i*DIM2+j+9 ) exit(67);

       if ( (dtr+k)->vdt1[j][i].var_a[i][j] != 1 ) exit(69);
       if ( (dtr+k)->vdt1[j][i].var_b[i][j] != i*DIM2+j+3 ) exit(71);
       if ( (dtr+k)->vdt1[j][i].var_c[i][j] != 'B' ) exit(73);
       if ( (dtr+k)->vdt1[j][i].var_d[i][j] != i*DIM2+j+5 ) exit(75);
       if ( (dtr+k)->vdt1[j][i].var_e[i][j] != i*DIM2+j+6 ) exit(77);
       if ( (dtr+k)->vdt1[j][i].var_f[i][j] != i*DIM2+j+7 ) exit(79);
       if ( (dtr+k)->vdt1[j][i].var_g[i][j] != 1 ) exit(81);
       if ( (dtr+k)->vdt1[j][i].var_h[i][j] != i*DIM2+j+9 ) exit(83);
     }
    }
   }

   free(dta);


   dta = malloc(sizeof(DT2)*DIM2*DIM1);

   for ( k = 0; k < DIM2*DIM1; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
      (dta+k)->var_a[i][j] = dt0.var_a[i][j];
      (dta+k)->var_b[i][j] = dt0.var_b[i][j];
      (dta+k)->var_c[i][j] = dt0.var_c[i][j];
      (dta+k)->var_d[i][j] = dt0.var_d[i][j];
      (dta+k)->var_e[i][j] = dt0.var_e[i][j];
      (dta+k)->var_f[i][j] = dt0.var_f[i][j];
      (dta+k)->var_g[i][j] = dt0.var_g[i][j];
      (dta+k)->var_h[i][j] = dt0.var_h[i][j];

      (dta+k)->vdt1[j][i].var_a[i][j] = dt0.vdt1[j][i].var_a[i][j];
      (dta+k)->vdt1[j][i].var_b[i][j] = dt0.vdt1[j][i].var_b[i][j];
      (dta+k)->vdt1[j][i].var_c[i][j] = dt0.vdt1[j][i].var_c[i][j];
      (dta+k)->vdt1[j][i].var_d[i][j] = dt0.vdt1[j][i].var_d[i][j];
      (dta+k)->vdt1[j][i].var_e[i][j] = dt0.vdt1[j][i].var_e[i][j];
      (dta+k)->vdt1[j][i].var_f[i][j] = dt0.vdt1[j][i].var_f[i][j];
      (dta+k)->vdt1[j][i].var_g[i][j] = dt0.vdt1[j][i].var_g[i][j];
      (dta+k)->vdt1[j][i].var_h[i][j] = dt0.vdt1[j][i].var_h[i][j];
     }
    }
   }

   dtb = malloc(sizeof(DT2)*DIM1*DIM2);
   dtp = malloc(sizeof(DT2)*DIM1*DIM2);
   dtp = st2sum(dta,dta);

   dtr = fun2(dtp,dtb);

   for ( k = 0; k < DIM2*DIM1; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
       if ( (dtb+k)->var_a[i][j] != (float)(2*(i*DIM2+j+2)-1) ) exit(85);
       if ( (dtb+k)->var_b[i][j] != 1 ) exit(87);
       if ( (dtb+k)->var_c[i][j] != (long double)(2*(i*DIM2+j+4)+1) ) exit(89);
       if ( (dtb+k)->var_d[i][j] != (double)(2*(i*DIM2+j+5)+2) ) exit(91);
       if ( (dtb+k)->var_e[i][j] != (float)(2*(i*DIM2+j+6)+3) ) exit(93);
       if ( (dtb+k)->var_f[i][j] != 1 ) exit(95);
       if ( (dtb+k)->var_g[i][j] != (double)(2*(i*DIM2+j+8)+5) ) exit(97);
       if ( (dtb+k)->var_h[i][j] != 'B' ) exit(99);

       if ( (dtr+k)->var_a[i][j] != (float)(2*(i*DIM2+j+2)-1) ) exit(101);
       if ( (dtr+k)->var_b[i][j] != 1 ) exit(103);
       if ( (dtr+k)->var_c[i][j] != (long double)(2*(i*DIM2+j+4)+1) ) exit(105);
       if ( (dtr+k)->var_d[i][j] != (double)(2*(i*DIM2+j+5)+2) ) exit(107);
       if ( (dtr+k)->var_e[i][j] != (float)(2*(i*DIM2+j+6)+3) ) exit(109);
       if ( (dtr+k)->var_f[i][j] != 1 ) exit(111);
       if ( (dtr+k)->var_g[i][j] != (double)(2*(i*DIM2+j+8)+5) ) exit(113);
       if ( (dtr+k)->var_h[i][j] != 'B' ) exit(115);

       if ( (dtb+k)->vdt1[j][i].var_a[i][j] != 1 ) exit(117);
       if ( (dtb+k)->vdt1[j][i].var_b[i][j] != 2*(i*DIM2+j+3) ) exit(119);
       if ( (dtb+k)->vdt1[j][i].var_c[i][j] != 'B' ) exit(121);
       if ( (dtb+k)->vdt1[j][i].var_d[i][j] != 2*(i*DIM2+j+5)+2 ) exit(123);
       if ( (dtb+k)->vdt1[j][i].var_e[i][j] != 2*(i*DIM2+j+6)+3 ) exit(125);
       if ( (dtb+k)->vdt1[j][i].var_f[i][j] != 2*(i*DIM2+j+7)+4 ) exit(127);
       if ( (dtb+k)->vdt1[j][i].var_g[i][j] != 1 ) exit(129);
       if ( (dtb+k)->vdt1[j][i].var_h[i][j] != 2*(i*DIM2+j+9)+6 ) exit(131);

       if ( (dtr+k)->vdt1[j][i].var_a[i][j] != 1 ) exit(133);
       if ( (dtr+k)->vdt1[j][i].var_b[i][j] != 2*(i*DIM2+j+3) ) exit(135);
       if ( (dtr+k)->vdt1[j][i].var_c[i][j] != 'B' ) exit(137);
       if ( (dtr+k)->vdt1[j][i].var_d[i][j] != 2*(i*DIM2+j+5)+2 ) exit(139);
       if ( (dtr+k)->vdt1[j][i].var_e[i][j] != 2*(i*DIM2+j+6)+3 ) exit(141);
       if ( (dtr+k)->vdt1[j][i].var_f[i][j] != 2*(i*DIM2+j+7)+4 ) exit(143);
       if ( (dtr+k)->vdt1[j][i].var_g[i][j] != 1 ) exit(145);
       if ( (dtr+k)->vdt1[j][i].var_h[i][j] != 2*(i*DIM2+j+9)+6 ) exit(147);
     }
    }
   }

   free(dta);
   free(dtb);
   free(dtp);

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

   dtp = malloc(sizeof(DT2)*DIM1*DIM2);

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

      dtq = st1sum(&(dtx+k)->vdt1[j][i],&(dty+k)->vdt1[j][i]);
      (dtp+k)->vdt1[j][i] = *dtq;
     }
    }
   }

   return(dtp);
}

