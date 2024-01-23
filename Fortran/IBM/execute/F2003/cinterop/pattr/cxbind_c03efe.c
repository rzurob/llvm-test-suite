/* C code for testcase fxbind_c03efe.f  */

#include <stdio.h>
#include <assert.h>

typedef struct robot ROBOT;

struct robot {
  
  float velocity;
  int energy;
  int IQ;
  char name;
};
int  swap_drt(ROBOT *,ROBOT *);  
void info(ROBOT *, ROBOT *); /* declare function */

int main() {
  ROBOT r;
  ROBOT r1 = {'E', 100, 231, 4.1}; /* initialize 2 ROBOTs */
  ROBOT r2 = {'T', 150, 254, 3.};
  ROBOT  *p1;  /* declare a structure pointer */  
  ROBOT  *p2;
  ROBOT  *p3;
  int x;
  p1 = &r1;
  p2 = &r2;
  p3 = &r;
  r = r2;
  x= swap_drt(p1, p2);
 
  info(p1,p3);
  return 0;
}

void info(ROBOT *x, ROBOT *y) {
  assert (x->name == y->name);
  assert (x->velocity == y->velocity);
  assert (x->IQ == y->IQ);
  assert (x->energy == y->energy);
}
