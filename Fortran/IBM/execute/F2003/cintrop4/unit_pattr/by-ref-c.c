#include <stdio.h>
#include <assert.h>

void decr (int *arg1);

int main()
{
   int v1 = 7;
   
   decr (&v1);
   assert (v1==6);
   return 0;
}
