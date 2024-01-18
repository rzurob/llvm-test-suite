#include <stdio.h>
#include <stdlib.h>

extern int foo1;
extern float foo2;
extern char foo3[1];

void cssub()
{
 printf(" foo1=%d\n foo2=%f\n foo3=%s\n", foo1, foo2, foo3);
 foo1 += 1;
 foo2 += 1;
 foo3[0] = 'C';
 printf(" foo1=%d\n foo2=%f\n foo3=%s\n", foo1, foo2, foo3);
 return;
}
