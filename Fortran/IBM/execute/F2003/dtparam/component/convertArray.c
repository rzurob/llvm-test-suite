#include <stdio.h>
#include <ctype.h>

void convertArray (char c[], int * i)
{
    int index;

    for (index = 0; index < *i; ++ index)
    {
        c[index] = toupper(c[index]);
    }
}
