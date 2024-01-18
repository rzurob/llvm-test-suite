
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>

int fun1(char *x, char *y) {

   if ( *x != '\0' ) exit(21);
   *y = '\0';
   return 0;
}

int fun2(char *x, char *y) {

   if ( *x != '\a' ) exit(23);
   *y = '\a';
   return 0;
}

int fun3(char *x, char *y) {

   if ( *x != '\b' ) exit(25);
   *y = '\b';
   return 0;
}

int fun4(char *x, char *y) {

   if ( *x != '\f' ) exit(27);
   *y = '\f';
   return 0;
}

int fun5(char *x, char *y) {

   if ( *x != '\n' ) exit(29);
   *y = '\n';
   return 0;
}

int fun6(char *x, char *y) {

   if ( *x != '\r' ) exit(31);
   *y = '\r';
   return 0;
}

int fun7(char *x, char *y) {

   if ( *x != '\t' ) exit(33);
   *y = '\t';
   return 0;
}

int fun8(char *x, char *y) {

   if ( *x != '\v' ) exit(35);
   *y = '\v';
   return 0;
}

int fun9(char *x) {
   if ( strcmp("string",x) ) exit(37);
   return 0;
}

int fun10(void **x) {

   if ( *x != NULL ) exit(39);
   if ( (short)*x != NULL ) exit(41);
   if ( (int)*x != NULL ) exit(43);
   if ( (long)*x != NULL ) exit(45);
   if ( (long long)*x != NULL ) exit(47);
   return 0;
}
