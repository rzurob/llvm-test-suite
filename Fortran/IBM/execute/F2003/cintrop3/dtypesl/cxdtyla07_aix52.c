
#include <stdio.h>
#include <stdlib.h>



typedef struct dt1 DT1;
typedef struct dt2 DT2;
typedef struct dt3 DT3;

struct dt1 {
   _Bool var_a;
   double var_b;
   char var_c;
   float var_d;
   long double var_e;
   double var_f;
   _Bool var_g;
   float var_h;
};

struct dt2 {
   double var_a;
   _Bool var_b;
   double var_c;
   char var_d;
   long double var_e;
   float var_f;
   _Bool var_g;
   float var_h;
   DT1 vdt1;
};

struct dt3 {
   DT2 vdt2;
   float var_a;
   _Bool var_b;
   long double var_c;
   double var_d;
   float var_e;
   _Bool var_f;
   double var_g;
   char var_h;
};

DT1 *st1sum(DT1 *dtx, DT1 *dty);
DT2 *st2sum(DT2 *dtx, DT2 *dty);
DT3 *st3sum(DT3 *dtx, DT3 *dty);

int main() {

   DT3 fun1(DT3 *dt);
   DT3 fun2(DT3 dt);
   DT3 fun3(DT3 *dtx, DT3 *dty);
   DT3 *fun4(DT3 *dtx, DT3 *dty);

   DT3 dt0 = {{2.0,0,6.0,'A',10.0l,12.0f,0,16.0f,
             {0,4.0,'A',8.0f,10.0l,12.0,0,16.0f}},
             2.0f,0,6.0l,8.0,10.0f,0,14.0,'A'};

   DT3 dta, dtb, dtc, *dtp;


   dta = dt0;

   dtb = fun1(&dta);

   if ( dta.var_a != 3.0f || dtb.var_a != 3.0f ) exit(21);
   if ( dta.var_b != 1 || dtb.var_b != 1 ) exit(23);
   if ( dta.var_c != 7.0l || dtb.var_c != 7.0l ) exit(25);
   if ( dta.var_d != 9.0 || dtb.var_d != 9.0 ) exit(27);
   if ( dta.var_e != 11.0f || dtb.var_e != 11.0f ) exit(29);
   if ( dta.var_f != 1 || dtb.var_f != 1 ) exit(31);
   if ( dta.var_g != 15.0 || dtb.var_g != 15.0 ) exit(33);
   if ( dta.var_h != 'B' || dtb.var_h != 'B' ) exit(35);

   if ( dta.vdt2.var_a != 4.0 || dtb.vdt2.var_a != 4.0 ) exit(37);
   if ( dta.vdt2.var_b != 1 || dtb.vdt2.var_b != 1 ) exit(39);
   if ( dta.vdt2.var_c != 8.0 || dtb.vdt2.var_c != 8.0 ) exit(41);
   if ( dta.vdt2.var_d != 'B' || dtb.vdt2.var_d != 'B' ) exit(43);
   if ( dta.vdt2.var_e != 12.0l || dtb.vdt2.var_e != 12.0l ) exit(45);
   if ( dta.vdt2.var_f != 14.0f || dtb.vdt2.var_f != 14.0f ) exit(47);
   if ( dta.vdt2.var_g != 1 || dtb.vdt2.var_g != 1 ) exit(49);
   if ( dta.vdt2.var_h != 18.0f || dtb.vdt2.var_h != 18.0f ) exit(51);

   if ( dta.vdt2.vdt1.var_a != 1 || 
                               dtb.vdt2.vdt1.var_a != 1 ) exit(53);
   if ( dta.vdt2.vdt1.var_b != 7.0 || 
                               dta.vdt2.vdt1.var_b != 7.0 ) exit(55);
   if ( dta.vdt2.vdt1.var_c != 'B' || 
                               dta.vdt2.vdt1.var_c != 'B' ) exit(57);
   if ( dta.vdt2.vdt1.var_d != 11.0f || 
                               dta.vdt2.vdt1.var_d != 11.0f ) exit(59);
   if ( dta.vdt2.vdt1.var_e != 13.0l || 
                               dta.vdt2.vdt1.var_e != 13.0l ) exit(61);
   if ( dta.vdt2.vdt1.var_f != 15.0 || 
                               dta.vdt2.vdt1.var_f != 15.0 ) exit(63);
   if ( dta.vdt2.vdt1.var_g != 1 || 
                               dta.vdt2.vdt1.var_g != 1 ) exit(65);
   if ( dta.vdt2.vdt1.var_h != 19.0f || 
                               dta.vdt2.vdt1.var_h != 19.0f ) exit(67);


   dta = dt0;

   dtb = fun2(dta);

   if ( dta.var_a != 2.0f || dtb.var_a != 3.0f ) exit(69);
   if ( dta.var_b != 0 || dtb.var_b != 1 ) exit(71);
   if ( dta.var_c != 6.0l || dtb.var_c != 7.0l ) exit(73);
   if ( dta.var_d != 8.0 || dtb.var_d != 9.0 ) exit(75);
   if ( dta.var_e != 10.0f || dtb.var_e != 11.0f ) exit(77);
   if ( dta.var_f != 0 || dtb.var_f != 1 ) exit(79);
   if ( dta.var_g != 14.0 || dtb.var_g != 15.0 ) exit(81);
   if ( dta.var_h != 'A' || dtb.var_h != 'B' ) exit(83);

   if ( dta.vdt2.var_a != 2.0 || dtb.vdt2.var_a != 4.0 ) exit(85);
   if ( dta.vdt2.var_b != 0 || dtb.vdt2.var_b != 1 ) exit(87);
   if ( dta.vdt2.var_c != 6.0 || dtb.vdt2.var_c != 8.0 ) exit(89);
   if ( dta.vdt2.var_d != 'A' || dtb.vdt2.var_d != 'B' ) exit(91);
   if ( dta.vdt2.var_e != 10.0l || dtb.vdt2.var_e != 12.0l ) exit(93);
   if ( dta.vdt2.var_f != 12.0f || dtb.vdt2.var_f != 14.0f ) exit(95);
   if ( dta.vdt2.var_g != 0 || dtb.vdt2.var_g != 1 ) exit(97);
   if ( dta.vdt2.var_h != 16.0f || dtb.vdt2.var_h != 18.0f ) exit(99);

   if ( dta.vdt2.vdt1.var_a != 0 || 
                               dtb.vdt2.vdt1.var_a != 1 ) exit(101);
   if ( dta.vdt2.vdt1.var_b != 4.0 || 
                               dtb.vdt2.vdt1.var_b != 7.0 ) exit(103);
   if ( dta.vdt2.vdt1.var_c != 'A' || 
                               dtb.vdt2.vdt1.var_c != 'B' ) exit(105);
   if ( dta.vdt2.vdt1.var_d != 8.0f || 
                               dtb.vdt2.vdt1.var_d != 11.0f ) exit(107);
   if ( dta.vdt2.vdt1.var_e != 10.0l || 
                               dtb.vdt2.vdt1.var_e != 13.0l ) exit(109);
   if ( dta.vdt2.vdt1.var_f != 12.0 || 
                               dtb.vdt2.vdt1.var_f != 15.0 ) exit(111);
   if ( dta.vdt2.vdt1.var_g != 0 || 
                               dtb.vdt2.vdt1.var_g != 1 ) exit(113);
   if ( dta.vdt2.vdt1.var_h != 16.0f || 
                               dtb.vdt2.vdt1.var_h != 19.0f ) exit(115);


   dta = dt0;
   dtp = malloc(sizeof(DT3));
   dtp = st3sum(&dta,&dta);

   dtc = fun3(dtp,&dtb);

   if ( dtb.var_a != 5.0f || dtc.var_a != 5.0f ) exit(117);
   if ( dtb.var_b != 1 || dtc.var_b != 1 ) exit(119);
   if ( dtb.var_c != 13.0l || dtc.var_c != 13.0l ) exit(121);
   if ( dtb.var_d != 17.0 || dtc.var_d != 17.0 ) exit(123);
   if ( dtb.var_e != 21.0f || dtc.var_e != 21.0f ) exit(125);
   if ( dtb.var_f != 1 || dtc.var_f != 1 ) exit(127);
   if ( dtb.var_g != 29.0 || dtc.var_g != 29.0 ) exit(129);
   if ( dtb.var_h != 'B' || dtc.var_h != 'B' ) exit(131);

   if ( dtb.vdt2.var_a != 6.0 || dtc.vdt2.var_a != 6.0 ) exit(133);
   if ( dtb.vdt2.var_b != 1 || dtc.vdt2.var_b != 1 ) exit(135);
   if ( dtb.vdt2.var_c != 14.0 || dtc.vdt2.var_c != 14.0 ) exit(137);
   if ( dtb.vdt2.var_d != 'B' || dtc.vdt2.var_d != 'B' ) exit(139);
   if ( dtb.vdt2.var_e != 22.0l || dtc.vdt2.var_e != 22.0l ) exit(141);
   if ( dtb.vdt2.var_f != 26.0f || dtc.vdt2.var_f != 26.0f ) exit(143);
   if ( dtb.vdt2.var_g != 1 || dtc.vdt2.var_g != 1 ) exit(145);
   if ( dtb.vdt2.var_h != 34.0f || dtc.vdt2.var_h != 34.0f ) exit(147);

   if ( dtb.vdt2.vdt1.var_a != 1 || 
                              dtc.vdt2.vdt1.var_a != 1 ) exit(149);
   if ( dtb.vdt2.vdt1.var_b != 11.0 || 
                              dtc.vdt2.vdt1.var_b != 11.0 ) exit(151);
   if ( dtb.vdt2.vdt1.var_c != 'B' || 
                              dtc.vdt2.vdt1.var_c != 'B' ) exit(153);
   if ( dtb.vdt2.vdt1.var_d != 19.0f || 
                              dtc.vdt2.vdt1.var_d != 19.0f ) exit(155);
   if ( dtb.vdt2.vdt1.var_e != 23.0l || 
                              dtc.vdt2.vdt1.var_e != 23.0l ) exit(157);
   if ( dtb.vdt2.vdt1.var_f != 27.0 || 
                              dtc.vdt2.vdt1.var_f != 27.0 ) exit(159);
   if ( dtb.vdt2.vdt1.var_g != 1 || 
                              dtc.vdt2.vdt1.var_g != 1 ) exit(161);
   if ( dtb.vdt2.vdt1.var_h != 35.0f || 
                              dtc.vdt2.vdt1.var_h != 35.0f ) exit(163);


   dta = dt0;
   dtp = malloc(sizeof(DT3));
   dtp = st3sum(&dta,&dta);

   dtp = fun4(dtp,&dtb);

   if ( dtb.var_a != 5.0f || dtp->var_a != 5.0f ) exit(165);
   if ( dtb.var_b != 1 || dtp->var_b != 1 ) exit(167);
   if ( dtb.var_c != 13.0l || dtp->var_c != 13.0l ) exit(169);
   if ( dtb.var_d != 17.0 || dtp->var_d != 17.0 ) exit(171);
   if ( dtb.var_e != 21.0f || dtp->var_e != 21.0f ) exit(173);
   if ( dtb.var_f != 1 || dtp->var_f != 1 ) exit(175);
   if ( dtb.var_g != 29.0 || dtp->var_g != 29.0 ) exit(177);
   if ( dtb.var_h != 'B' || dtp->var_h != 'B' ) exit(179);

   if ( dtb.vdt2.var_a != 6.0 || dtp->vdt2.var_a != 6.0 ) exit(181);
   if ( dtb.vdt2.var_b != 1 || dtp->vdt2.var_b != 1 ) exit(183);
   if ( dtb.vdt2.var_c != 14.0 || dtp->vdt2.var_c != 14.0 ) exit(185);
   if ( dtb.vdt2.var_d != 'B' || dtp->vdt2.var_d != 'B' ) exit(187);
   if ( dtb.vdt2.var_e != 22.0l || dtp->vdt2.var_e != 22.0l ) exit(189);
   if ( dtb.vdt2.var_f != 26.0f || dtp->vdt2.var_f != 26.0f ) exit(191);
   if ( dtb.vdt2.var_g != 1 || dtp->vdt2.var_g != 1 ) exit(193);
   if ( dtb.vdt2.var_h != 34.0f || dtp->vdt2.var_h != 34.0f ) exit(195);

   if ( dtb.vdt2.vdt1.var_a != 1 || 
                              dtp->vdt2.vdt1.var_a != 1 ) exit(197);
   if ( dtb.vdt2.vdt1.var_b != 11.0 || 
                              dtp->vdt2.vdt1.var_b != 11.0 ) exit(199);
   if ( dtb.vdt2.vdt1.var_c != 'B' || 
                              dtp->vdt2.vdt1.var_c != 'B' ) exit(201);
   if ( dtb.vdt2.vdt1.var_d != 19.0f || 
                              dtp->vdt2.vdt1.var_d != 19.0f ) exit(203);
   if ( dtb.vdt2.vdt1.var_e != 23.0l || 
                              dtp->vdt2.vdt1.var_e != 23.0l ) exit(205);
   if ( dtb.vdt2.vdt1.var_f != 27.0 || 
                              dtp->vdt2.vdt1.var_f != 27.0 ) exit(207);
   if ( dtb.vdt2.vdt1.var_g != 1 || 
                              dtp->vdt2.vdt1.var_g != 1 ) exit(209);
   if ( dtb.vdt2.vdt1.var_h != 35.0f || 
                              dtp->vdt2.vdt1.var_h != 35.0f ) exit(211);

   return 0;
}

DT3 *st3sum(DT3 *dtx, DT3 *dty) {
   DT3 *dtp;
   DT2 *dtq;

   dtp = malloc(sizeof(DT3));

   dtp->var_a = dtx->var_a + dty->var_a;
   dtp->var_b = dtx->var_b && dty->var_b;
   dtp->var_c = dtx->var_c + dty->var_c;
   dtp->var_d = dtx->var_d + dty->var_d;
   dtp->var_e = dtx->var_e + dty->var_e;
   dtp->var_f = dtx->var_f && dty->var_f;
   dtp->var_g = dtx->var_g + dty->var_g;
   dtp->var_h = dtx->var_h;
   dtq = st2sum(&dtx->vdt2,&dty->vdt2);
   dtp->vdt2 = *dtq;

   return(dtp);
}

DT2 *st2sum(DT2 *dtx, DT2 *dty) {
   DT2 *dtp;
   DT1 *dtq;

   dtp = malloc(sizeof(DT2));

   dtp->var_a = dtx->var_a + dty->var_a;
   dtp->var_b = dtx->var_b && dty->var_b;
   dtp->var_c = dtx->var_c + dty->var_c;
   dtp->var_d = dtx->var_d;
   dtp->var_e = dtx->var_e + dty->var_e;
   dtp->var_f = dtx->var_f + dty->var_f;
   dtp->var_g = dtx->var_g && dty->var_g;
   dtp->var_h = dtx->var_h + dty->var_h;
   dtq = st1sum(&dtx->vdt1,&dty->vdt1);
   dtp->vdt1 = *dtq;

   return(dtp);
}

DT1 *st1sum(DT1 *dtx, DT1 *dty) {
   DT1 *dtp;

   dtp = malloc(sizeof(DT1));

   dtp->var_a = dtx->var_a && dty->var_a;
   dtp->var_b = dtx->var_b + dty->var_b;
   dtp->var_c = dtx->var_c;
   dtp->var_d = dtx->var_d + dty->var_d;
   dtp->var_e = dtx->var_e + dty->var_e;
   dtp->var_f = dtx->var_f + dty->var_f;
   dtp->var_g = dtx->var_g && dty->var_g;
   dtp->var_h = dtx->var_h + dty->var_h;

   return(dtp);
}
