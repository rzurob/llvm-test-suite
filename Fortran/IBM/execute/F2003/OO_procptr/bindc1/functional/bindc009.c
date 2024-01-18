
void printinteger ( int i1[3], short int i2[][3] )
{
   printf("here are the elements of i1[0-2]: %d, %d, %d\n", i1[0], i1[1], i1[2] );
   printf("here are the elements of i2[0][0-2]: %d, %d, %d\n", i2[0][0], i2[0][1], i2[0][2] );
   printf("here are the elements of i2[1][0-2]: %d, %d, %d\n", i2[1][0], i2[1][1], i2[1][2] );
}

void addarray ( int i1[3], short int i2[][3] )
{
   printf("here are the elements of i1[0-2]: %d, %d, %d\n", i1[0], i1[1], i1[2] );
   printf("here are the elements of i2[0][0-2]: %d, %d, %d\n", i2[0][0], i2[0][1], i2[0][2] );
   printf("here are the elements of i2[1][0-2]: %d, %d, %d\n", i2[1][0], i2[1][1], i2[1][2] );
   printf("here are the elements of SUM of i1[0-2] and i2[1][0-2]: %d, %d, %d\n", i1[0] + i2[1][0], i1[1] + i2[1][1], i1[2] + i2[1][2] );
}
