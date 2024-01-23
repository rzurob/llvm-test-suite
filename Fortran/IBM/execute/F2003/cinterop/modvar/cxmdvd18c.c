#include <inttypes.h>
#include <stdio.h>

/*
  C code for testcase "fxmdvd18.f"
*/

#include <stdio.h>


struct dt0 {
  int a;
  int_fast32_t b;
};

struct dt1 {
  int a;
  int_fast32_t b;
  struct dt0 d0;
};

struct dt2 {
  int a;
  int_fast32_t b;
  struct dt1 d1;
};

int csub3(struct dt2 * x)
{
  /*Define the stuct to store the correct value used for verificaton. */

  struct dt2 correct_value1;
  printf("x->a = %c\nx->b = %d\n", x->a, x->b);
  correct_value1.a = 5;
  correct_value1.b = 10;

  correct_value1.d1.a = 5;
  correct_value1.d1.b = 10;

  correct_value1.d1.d0.a = 5;
  correct_value1.d1.d0.b = 10;

  if ((x->a != correct_value1.a )||  (x->b != correct_value1.b))
    exit (2);

  else if ((x->d1.a != correct_value1.d1.a )||(x->d1.b != correct_value1.d1.b))
    exit (3);

  else if ((x->d1.d0.a != correct_value1.d1.d0.a )||(x->d1.d0.b != correct_value1.d1.d0.b))
    exit (4);

  else
    {
      x->a = 2;
      x->b = 4;
      printf("x->a = %d\n x->b = %d\n", x->a, x->b);
      x->d1.a = 2;
      x->d1.b = 4;
  
      x->d1.d0.a = 2;
      x->d1.d0.b = 4;
    }
  return 0;

}
