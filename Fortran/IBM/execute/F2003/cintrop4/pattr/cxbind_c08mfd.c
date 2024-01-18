/* C code for testcase  fxbind_c08mfd.f */

#include <stdio.h>
#include <stdlib.h>
/* openfile is implemented in Fortran Code */
void  openfile(void);
int main()
{
  FILE *my_stream;
  char my_filename[] = "test.txt";
  int flush_status;
  int close_error;

  if((my_stream = fopen(my_filename, "w"))==NULL) {
    printf("Cannot open file.\n");
    exit(1);
  }

  putc ('I', my_stream);
 

  
  /*  Since the stream is fully-buffered by default, not line-buffered, */
  /*      it needs to be flushed periodically.  will flush it here, */
  /*      even though  about to close it. */

  flush_status = fflush (my_stream);
  if (flush_status != 0)
    {
      puts ("Error flushing stream!");
    }
  else
    {
      puts ("Stream flushed.");
    }

  /* Close stream;  */
  openfile() ;
  
  close_error = fclose (my_stream);

  if (close_error != 0)
    {
      printf ("File could not be closed.\n");
    }
  else
    {
      printf ("File closed.\n");
    }

 
  return 0;
}
