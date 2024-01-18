#include <stdio.h>
void c_func_arr_sum(void* a, void* b)
{
  int i = 0;
  int ln;
  int res = 0;

  int *arr1 = (int *)a;
  int *arr2 = (int *)b;

  res += (*arr1 + *arr2);
  printf("Result is : %d\n", res);
  return;
}
