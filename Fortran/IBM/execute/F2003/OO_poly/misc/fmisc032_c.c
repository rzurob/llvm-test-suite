#include <stdio.h>
#include <unistd.h>

int main ()
{
    char * argv[] = {"fmisc032",NULL};

    if (fclose (stdin) != 0) exit (2);
    if (fclose (stdout) != 0) exit (3);
    if (fclose (stderr) != 0) exit (4);

    execv ("fmisc032", argv);

    exit (-1);
}
