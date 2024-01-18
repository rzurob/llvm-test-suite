
#include <stdio.h>
#include <stdlib.h>

void csub(void(**f)(char *, char *)) {

   extern void swap(char *, char *);

   *f = &swap;

}

void  swap(char *p1, char *p2) {

   char tmp;

   if (*p1 != 'A' ) exit(46);
   if (*p2 != 'Z' ) exit(47);

   tmp = *p2;
   *p2 = *p1;
   *p1 = tmp;

}

