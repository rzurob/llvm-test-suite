/* The C code for testcase "fxbind_c04rrd.f"*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>

/* C struct */
typedef struct people
{
  float  weight;
  signed int  age;
  char sex;
} mycperson;

float stats(mycperson * , signed int *,float *, float *);

int main()
{
  /* Variable Declarations */

  signed int  ret,no;
  float m_a,f_a;
  mycperson girls[] = { 
    50.,15,'f',
    60.,15,'m',
    40.,18,'f',
    70.,20,'m',
  };
  no = 4;
  ret =(int) stats (&girls[0],&no,&m_a,&f_a);
  printf("The value of m_a is %f",m_a);
  printf("The value of ret is %d", ret);
  assert (ret == 65);  
  return 0;
}
