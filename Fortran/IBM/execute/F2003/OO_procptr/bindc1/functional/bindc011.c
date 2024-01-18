typedef struct
{
   int i;
   double r;
   char c[3];

}base;

void print1 ( base dtv[3] )
{
   printf("here is element[0]: %d, %f, %c, %c, %c\n", dtv[0].i, dtv[0].r, dtv[0].c[0], dtv[0].c[1], dtv[0].c[2] );
   printf("here is element[1]: %d, %f, %c, %c, %c\n", dtv[1].i, dtv[1].r, dtv[1].c[0], dtv[1].c[1], dtv[1].c[2] );
   printf("here is element[2]: %d, %f, %c, %c, %c\n", dtv[2].i, dtv[2].r, dtv[2].c[0], dtv[2].c[1], dtv[2].c[2] );
}

base add ( base dtv1[3], base dtv2[][3] )
{
   base tmp;

   printf("here is element[0] for dtv1: %d, %f, %c, %c, %c\n", dtv1[0].i, dtv1[0].r, dtv1[0].c[0], dtv1[0].c[1], dtv1[0].c[2] );
   printf("here is element[1] for dtv1: %d, %f, %c, %c, %c\n", dtv1[1].i, dtv1[1].r, dtv1[1].c[0], dtv1[1].c[1], dtv1[1].c[2] );
   printf("here is element[2] for dtv1: %d, %f, %c, %c, %c\n", dtv1[2].i, dtv1[2].r, dtv1[2].c[0], dtv1[2].c[1], dtv1[2].c[2] );

   printf("here is element[0][0] for dtv2: %d, %f, %c, %c, %c\n", dtv2[0][0].i, dtv2[0][0].r, dtv2[0][0].c[0], dtv2[0][0].c[1], dtv2[0][0].c[2] );
   printf("here is element[0][1] for dtv2: %d, %f, %c, %c, %c\n", dtv2[0][1].i, dtv2[0][1].r, dtv2[0][1].c[0], dtv2[0][1].c[1], dtv2[0][1].c[2] );
   printf("here is element[0][2] for dtv2: %d, %f, %c, %c, %c\n", dtv2[0][2].i, dtv2[0][2].r, dtv2[0][2].c[0], dtv2[0][2].c[1], dtv2[0][2].c[2] );

   printf("here is element[1][0] for dtv2: %d, %f, %c, %c, %c\n", dtv2[1][0].i, dtv2[1][0].r, dtv2[1][0].c[0], dtv2[1][0].c[1], dtv2[1][0].c[2] );
   printf("here is element[1][1] for dtv2: %d, %f, %c, %c, %c\n", dtv2[1][1].i, dtv2[1][1].r, dtv2[1][1].c[0], dtv2[1][1].c[1], dtv2[1][1].c[2] );
   printf("here is element[1][2] for dtv2: %d, %f, %c, %c, %c\n", dtv2[1][2].i, dtv2[1][2].r, dtv2[1][2].c[0], dtv2[1][2].c[1], dtv2[1][2].c[2] );

   tmp.i = dtv1[0].i + dtv2[0][0].i;
   tmp.r = dtv1[1].i + dtv2[1][0].i;
   tmp.c[0] = dtv1[2].c[0];
   tmp.c[1] = dtv1[2].c[1];
   tmp.c[2] = dtv1[2].c[2];

   return tmp;
}
