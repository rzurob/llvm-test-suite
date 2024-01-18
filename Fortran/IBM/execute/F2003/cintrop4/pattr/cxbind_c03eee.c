/* C code for testcase fxbind_c03eee.f  */

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
int ent_swap_drt (ROBOT *) ;
void info(ROBOT *, ROBOT *); /* declare function */

int main() {
  ROBOT r;
  ROBOT r1 = { 4.1,231,100,'E'}; /* initialize 2 ROBOTs */
  ROBOT r2 = { 6.1,254,150,'T'};
  ROBOT r1_ref={16.1,264,160,'U'};
  ROBOT  *p1;  /* declare a structure pointer */  
  ROBOT  *p2;
  ROBOT  *p3;
  ROBOT  *p4;
  int x;
  p1 = &r1;
  p2 = &r2;
  p3 = &r;
  p4 = &r1_ref;
  r = r2;
  x= swap_drt(p1, p2);
 
  info(p1,p3);

  x = ent_swap_drt (p1) ;
  info(p4,p1); 
  assert ( x== p4->IQ);
  return 0;
}

void info(ROBOT *x, ROBOT *y) {
  assert (x->name == y->name);
  assert (x->velocity == y->velocity);
  assert (x->IQ == y->IQ);
  assert (x->energy == y->energy);
}
