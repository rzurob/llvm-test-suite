#include <stdio.h>
#include <strings.h>


char FortString[50];

void printFORTStr (char s[])
{
    printf("%s\n", s);
    fflush(stdout);
}


int copyStr (char s[], int len)
{
    memset (FortString, 0, sizeof(FortString));
    strncpy (FortString, s, len);

    return 1;
}
