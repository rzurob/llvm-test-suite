
#include <stdio.h>
#include <stdlib.h>


void * match(char c, char *s){

  while(c!=*s && *s) s++;

  return(s);

}
