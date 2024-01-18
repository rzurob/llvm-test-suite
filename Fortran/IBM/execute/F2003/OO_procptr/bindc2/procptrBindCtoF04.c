#include <stdio.h>
#include <stdlib.h>


struct dt0 {
   int a0[5];
};

struct dt1 {
   int a1[5];
   struct dt0 d0;
};

struct dt2 {
   int a2[5];
   struct dt1 d1;
};

void initdt0(void *);
void initdt1(void *);
void initdt2(void *);

int main() {

   int fnt0(struct dt0 *);
   int fnt1(struct dt1 *);
   int fnt2(struct dt2 *);

   struct dt0 dta;
   struct dt1 dtb;
   struct dt2 dtc;

   int i, ret;

   ret = fnt0(&dta);

   for ( i = 0; i < 5; i++ ) if ( dta.a0[i] != i+2 ) exit(71);

   ret = fnt1(&dtb);

   for ( i = 0; i < 5; i++ ) if ( dtb.a1[i] != i+2 ) exit(72);
   for ( i = 0; i < 5; i++ ) if ( dtb.d0.a0[i] != i+2 ) exit(73);

   ret = fnt2(&dtc);

   for ( i = 0; i < 5; i++ ) if ( dtc.a2[i] != i+2 ) exit(74);
   for ( i = 0; i < 5; i++ ) if ( dtc.d1.a1[i] != i+2 ) exit(75);
   for ( i = 0; i < 5; i++ ) if ( dtc.d1.d0.a0[i] != i+2 ) exit(76);

   ret = fnt(&dtc);

   for ( i = 0; i < 5; i++ ) if ( dtc.a2[i] != 23 ) exit(77);
   for ( i = 0; i < 5; i++ ) if ( dtc.d1.a1[i] != 33 ) exit(78);
   for ( i = 0; i < 5; i++ ) if ( dtc.d1.d0.a0[i] != 43 ) exit(79);

   return 0;
}


void initdt0(void *x) {
   int i;

   struct dt0 *q;

   q = malloc(sizeof(struct dt0));

   if(!(q=malloc(sizeof(struct dt0)))){
      printf("Out of memory.\n");
      exit(80);
   }

   *q = * (struct dt0 *) x;

   for ( i = 0; i < 5; i++ ) {
     printf("%d\n", q->a0[i]);
     if ( q->a0[i] != 2 ) exit(81);
   }

   for ( i = 0; i < 5; i++ ) {
          q->a0[i] = i+1;
   }

   x = q;

}


void initdt1(void *x) {
   int i;

   struct dt1 *q;

   q = malloc(sizeof(struct dt1));

   if(!(q=malloc(sizeof(struct dt1)))){
      printf("Out of memory.\n");
      exit(82);
   }

   *q = * (struct dt1 *) x;

   for ( i = 0; i < 5; i++ ) {
     if ( (q)->a1[i] != 2 ) exit(83);
   }

   for ( i = 0; i < 5; i++ ) {
          (q)->a1[i] = i+1;
   }

   x = q;

   initdt0(&q->d0);
}

void initdt2(void *x) {
   int i;

   struct dt2 *q;

   q = malloc(sizeof(struct dt2));

   if(!(q=malloc(sizeof(struct dt2)))){
      printf("Out of memory.\n");
      exit(84);
   }

   *q = * (struct dt2 *) x;

   for ( i = 0; i < 5; i++ ) {
     if ( (q)->a2[i] != 2 ) exit(85);
   }

   for ( i = 0; i < 5; i++ ) {
          (q)->a2[i] = i+1;
   }

   x = q;

   initdt1(&q->d1);   

}


void update(void(**f)(struct dt2 *x)) {

    extern void updatedt(struct dt2 *x);

    *f = &updatedt;
}

void updatedt(struct dt2 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
          x->a2[i] = 23;
          x->d1.a1[i] = 33; 
          x->d1.d0.a0[i] = 43;
   }

}

