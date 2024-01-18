#include <inttypes.h>
#include <stdio.h>

/*
  C code for testcase "fxmdvd15.f"
*/

#include <stdio.h>


struct dt0 {
  int a;
  int_least64_t b;
};

struct dt1 {
  int a;
  int_least64_t b;
  struct dt0 d0;
};

struct dt2 {
  int a;
  int_least64_t b;
  struct dt1 d1;
};

int csub1(struct dt0 * x)
{
  /*Define the stuct to store the correct value used for verificaton. */

  struct dt0 correct_value1;
  printf("x->a = %c\nx->b = %d\n", x->a, x->b);
  correct_value1.a = 5;
  correct_value1.b = 10;

  if ((x->a == correct_value1.a ) && (x->b == correct_value1.b))
    {
      x->a = 2;
      x->b = 4;
      printf("x->a = %d\n x->b = %d\n", x->a, x->b);
    }
  else
    exit (1);
}
