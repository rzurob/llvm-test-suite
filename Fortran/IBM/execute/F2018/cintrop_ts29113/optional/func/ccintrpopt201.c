#include <inttypes.h>
#include <stdio.h>

void c_sub_test1(int *a, float *b)
{
  printf ("In c_sub_test1 \n");
  if (a != NULL)
    printf("a = %d\n", *a);
  else
    printf("a = absent \n");

  if (b != NULL)
    printf("b = %f\n", *b);
  else
    printf("b = absent\n");

}

size_t c_func_test2(int64_t *a)
{
  printf ("In c_func_test2 \n");

  if (a != NULL) {
    printf("a = %ld\n", *a);
    return sizeof(*a);
  }
  else {
    printf("a = absent\n");
    return 0;
  }
}
