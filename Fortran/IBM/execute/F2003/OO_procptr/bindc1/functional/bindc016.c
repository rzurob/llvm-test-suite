void setandprint1 ( float *r1, float *r2 )
{
   printf("before setting  : %f\n", *r1);
   *r1 =*r2;
   printf("after setting   : %f\n", *r1);
}

void setandprint2 ( float *r1, float *r2 )
{
   printf("===== setandprint2 =====\n");
   printf("before setting  : %f\n", *r1);
   *r1 =*r2;
   printf("after setting   : %f\n", *r1);
   printf("===== end of setandprint2 =====\n");
}
