#include<stdio.h>
#include<unistd.h>



int main(int argc, char ** argv, char ** envp)
{
/*

The main idea for this test is to form maximum length of a command line.
Different os has defferent length limit.
To issue a command, we can call execlp that will be independent from shell
To determine the maximum possible length, we should calculate
 
    limit - the size used for environment variables

The command format formed is:
  
    fxclms050 OptionLength xx.......x 

*/

  const char* p1="fxclms050";
  char  para2[500000];
  char  c; 
  int   rc;
  int   i;
  int   cnt;
  int   length;
  char  para1[8];

  while (*envp != 0)
  {
    cnt += strlen(*envp) + 1;
    envp++;
  }
 
/* 
 AIX:   /usr/include/sys/limits.h      24k = 24576
 MACOS: /usr/include/sys/syslimits.h   64k = 65536
 SLES:  /usr/include/linux/limits.h    128k= 131072
 RHEL:  /usr/include/linux/limits.h    128k= 131072

 isAIX:
     0)  OS="AIX"
     1)  OS="LINUX"
     2)  OS="MACOSX"
*/
  switch (atoi(*(++argv)))
  {
   case 0:
     length = 24576;
     break;
   case 1:
     length = 131072; 
     break;
   case 2:
     length = 65536; 
     break;
   default:
     printf("Unknown OS!\n");
     return 11;
  }

  cnt = length - cnt - (strlen(p1) +1 ) - 8;
  sprintf(para1, "%d", cnt);

  for (i=0; i < cnt; i++)
  {
    para2[i] = 'x';
  }
  para2[cnt] = '\0';

  rc = execlp(p1, p1, para1, para2, (char *)0);

  printf("ret is %d\n", rc );

}
