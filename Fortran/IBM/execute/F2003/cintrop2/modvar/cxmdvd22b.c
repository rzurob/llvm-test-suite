#include <inttypes.h>
#include <stdio.h>

/*
  C code for testcase "fxmdvd22.f"
*/

#include <stdio.h>


struct dt0 {
  int a;
  long long b;
};

struct dt1 {
  int a;
  long long b;
  struct dt0 d0;
};

struct dt2 {
  int a;
  long long b;
  struct dt1 d1;
};

int csub2(struct dt1 * x)
{
  /*Define the stuct to store the correct value used for verificaton. */

  struct dt1 correct_value1;
  printf("x->a = %c\nx->b = %d\n", x->a, x->b);
  correct_value1.a = 5;
  correct_value1.b = 10;

  correct_value1.d0.a = 5;
  correct_value1.d0.b = 10;

  if ((x->a != correct_value1.a )||  (x->b != correct_value1.b))
    exit (2);

  else if ((x->d0.a != correct_value1.d0.a )||(x->d0.b != correct_value1.d0.b))
    exit (3);

  else
    {
      x->a = 2;
      x->b = 4;
      printf("x->a = %d\n x->b = %d\n", x->a, x->b);
      x->d0.a = 2;
      x->d0.b = 4;
    }
  return 0;
}