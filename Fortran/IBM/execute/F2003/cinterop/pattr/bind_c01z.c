#include "stdio.h"

void extsub_int(signed char **i)
{ 
    **i = 15;
}

void extsub_log(unsigned char **i)
{  
    **i = 0;
}

void extsub_char(char **i)
{
    **i = 'a';
}

void extsub_real(float **i)
{
    **i = **i * 2;
}
