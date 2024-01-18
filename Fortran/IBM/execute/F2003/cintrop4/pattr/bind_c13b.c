#include <stdio.h>
#include <stdlib.h>

int ret;

int f(int x)
{ 
   return x * 2;
}

extern void fsub(int (*g)(int) );

void csub(int (*g)(int) ) 
{
   ret = g(2);
}

int main() {

   fsub(&f);
   
   if (ret != 4) exit(10);
   
   return 0; 

}
