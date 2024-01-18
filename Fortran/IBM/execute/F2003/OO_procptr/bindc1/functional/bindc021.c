void C_ESarray( int i1[4], float r1[4] )
{
   printf ("===   Inside C_ESarray   ===\n" );
   printf ("This is i1: %d %d %d %d\n", i1[0], i1[1], i1[2], i1[3] );
   printf ("This is r1: %f %f %f %f\n", r1[0], r1[1], r1[2], r1[3] );
}

void C_ASarray( int i1[], float r1[][2] )
{
   printf ("===   Inside C_ASarray   ===\n" );
   printf ("This is i1: %d %d %d %d\n", i1[0], i1[1], i1[2], i1[3] );
   printf ("This is r1: %f %f %f %f %f %f\n", r1[0][0], r1[0][1], r1[1][0], r1[1][1], r1[2][0], r1[2][1] );
}
