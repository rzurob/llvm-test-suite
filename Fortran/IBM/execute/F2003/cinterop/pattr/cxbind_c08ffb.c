/* C code for testcase "fxbind_c08ffb.f" */

#include <stdio.h>
#include <stdlib.h>
#define MAX 10
void  create_file()

{
  FILE *f;
  int x;
  
  /* Ensure the old file already been deleted by Fortran Code. */
  /* If not , delete it in C but still return 1 as error code. */
  f = fopen("fxbind_c08ffb.dat", "r");
  if(f != NULL)
    {
      printf("remove file  \n");

      if(!remove("fxbind_c08ffb.dat"))
        {       
         printf("Successful deletion in C but not in Fortran, check fortran code");
         fclose(f);
       
       }
      else
       printf("Oops, file not deleted (is it there?)");
      exit (1);
    }

  f=fopen("fxbind_c08ffb.dat","w");
  if (!f)
    exit (1);
  for(x=1; x<=MAX; x++)
    fprintf(f,"%d\n",x);
  fclose(f);
  
  
}

