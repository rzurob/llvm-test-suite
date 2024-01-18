
typedef struct
{
   int i;
   float r;
}cbase;


void C_printarray ( cbase b1[4], cbase b2[][2] )
{
   printf ("===   Inside C_printarray   ===\n" );
   printf ("This is b1.i: %d %d %d %d\n", b1[0].i, b1[1].i, b1[2].i, b1[3].i );
   printf ("This is b1.r: %f %f %f %f\n", b1[0].r, b1[1].r, b1[2].r, b1[3].r );
   printf ("This is b2.i: %d %d %d %d\n", b2[0][0].i, b2[0][1].i, b2[1][0].i, b2[1][1].i );
   printf ("This is b2.r: %f %f %f %f\n", b2[0][0].r, b2[0][1].r, b2[1][0].r, b2[1][1].r );
}