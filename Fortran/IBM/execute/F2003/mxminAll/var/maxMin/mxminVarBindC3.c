void sub1 ( char cc[5][3] )
{
   int i, j;

   if(cc[0][0] != 'f') exit(31);
   if(cc[0][1] != 'f') exit(32);
   if(cc[0][2] != '\0') exit(33);

   if(cc[1][0] != 'a') exit(34);
   if(cc[1][1] != 'a') exit(35);
   if(cc[1][2] != '\0') exit(36);

   if(cc[2][0] != 'x') exit(37);
   if(cc[2][1] != 'x') exit(38);
   if(cc[2][2] != '\0') exit(39);

   if(cc[3][0] != 'a') exit(41);
   if(cc[3][1] != 'a') exit(42);
   if(cc[3][2] != '\0') exit(43);

   if(cc[4][0] != 'h') exit(44);
   if(cc[4][1] != 'h') exit(45);
   if(cc[4][2] != '\0') exit(46);

}

