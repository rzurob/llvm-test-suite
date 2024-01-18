#include <stdio.h>
void decr (int arg1);

int main()
{
   int v1 = 7;

   printf ("C pre:  v1=%d\n", v1);
   decr (v1);
   printf ("C post: v1=%d\n", v1);
}
