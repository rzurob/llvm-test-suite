#include <stdio.h>
#include <stdint.h>

typedef struct dt1 DT1;

struct dt1 {
  int i2[2][2];
};

void f_func_test(int *, int_least8_t *);

void c_func_test(int * a, DT1 * b)
{
  int NUMBER = 10;
  int i, j;
  int_least8_t iarr[NUMBER];
 
  if (a != NULL) {
    *a = *a + 10;
    printf("a = %d\n", *a);
  }
  else
    printf("a = absent\n");

  if (b != NULL) {
    for (i=0; i<2; i++)
      for (j=0; j<2; j++)
    	printf("%d,", b->i2[i][j]);
    printf("\n");
  }
  else
    printf("b = absent\n");

  f_func_test(NULL, NULL);
  for (i=0; i<NUMBER; i++)
	iarr[i] = NUMBER + i;	

  f_func_test(&NUMBER,iarr);

  return;
}
