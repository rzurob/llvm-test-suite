
void printthem(float i, float j, float k)
{
   printf("Here are the values: %f, %f, %f \n", i, j, k);
}

float sumthem (float i, float j, float k )
{
   printf("Here are the values: %f, %f, %f \n", i, j, k);
   return i+j+k;
}

float subtractthem (float i, float j, float k )
{
   printf("Here are the values: %f, %f, %f \n", i, j, k);
   return -1*(sumthem(i,j,k));
}


