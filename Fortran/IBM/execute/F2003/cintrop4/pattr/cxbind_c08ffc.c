/* C code for testcase "fxbindc08ffc.f" */

#include <stdio.h>
#include <assert.h>

void creat_file() ;

int main()
{ 
  int sum = 0, val;
  FILE *ifp ;
  creat_file;
  ifp = fopen ("fxbind_c08ffc.dat","r");
  while (fscanf(ifp,"%d",&val)==1)
    sum += val;
  assert ( sum == 210 ) ;
  return 0;
}
