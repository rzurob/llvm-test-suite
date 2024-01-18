#include <stdio.h>
#include <string.h>

int sindex(char source[], char source2[], int no_bytes);


int sindex(char s[], char s2[], int no_byts)
{
        
	char *ip;
	int i;
	char r;
	ip = &s[0];
        ip = ip + no_byts - 1 ; 
        if ( *ip == '\0') return 1;
        ip = &s[0];
        ip = ip + no_byts; 
        r  =  *ip ;
        if ( *ip == '\0') return 0;

        r  =  *ip ;
	return r;
}
