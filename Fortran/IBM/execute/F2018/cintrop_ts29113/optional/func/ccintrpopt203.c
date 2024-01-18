#include <stdio.h>
#include <stdint.h>

typedef struct dt1 DT1;
typedef struct dt2 DT2;

struct dt1 {
    long long int a;
    int16_t b[2];
};

struct dt2 {
    float   c[2];
    struct dt1 d1;
};


void c_sub_test1(DT1 a[5], short int *b)
{
  int i, j;

  printf ("In c_sub_test1 \n");
  if (a != NULL) {
    for (i=0; i<5; i++) 
         printf("DT1[%d]:(%lld, (%d, %d)),  ", i, a[i].a, a[i].b[0], a[i].b[1]);
    printf ("\n");
  }
  else
    printf("a = absent \n");

  if (b != NULL)
    printf("b = %d\n", *b);
  else
    printf("b = absent\n");

}

size_t c_func_test2(DT2 a[3][5])
{
  int i, j, k;

  printf ("In c_func_test2 \n");

  if (a != NULL) {
    for (i=0; i<3; i++) 
    {
      for (j=0; j<5; j++) 
      {
        printf ("DT2[%d][%d]: ((", i, j);

        for (k=0; k<2; k++) 
   	  printf ("%f, ", a[i][j].c[k]);

   	printf ("), %lld, ", a[i][j].d1.a);

        for (k=0; k<2; k++)
   	  printf ("%d, ", a[i][j].d1.b[k]);

        printf (") \n");
      }
    }
    return sizeof(a[3][5]);
  }
  else {
    printf("a = absent\n");
    return 0;
  }
}
