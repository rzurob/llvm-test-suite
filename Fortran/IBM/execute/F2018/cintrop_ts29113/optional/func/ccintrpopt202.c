#include <stdio.h>

typedef struct dt1 DT1;

struct dt1 {
  double a;
  char b;
};

int realfunc(float a, char b, char c) ;
void c_sub_test1(int (*pt2Func)(float, char, char), DT1 dt_arr[3][3]);

void c_sub_test1(int (*pt2Func)(float, char, char), DT1 dt_arr[3][3])
{
 int i, j, result;

 if (dt_arr != NULL) {
    for (i=0; i<3; i++) {
	for (j=0; j<3; j++) {
		printf("(%f, %c) ", dt_arr[i][j].a, dt_arr[i][j].b);
        }
    }
    printf ("\n");	
 } else {
    printf ("dt_arr = absent\n");
 }
 
 if (pt2Func  != NULL) {
     if (dt_arr != NULL)
         result = pt2Func(12, dt_arr[0][0].b, 'b');
     else
         result = pt2Func(12, 'a', 'b');
 
         printf ("result=%d\n", result);
 } else {
         printf ("pt2Func = absent\n");
 }

}


int realfunc(float a, char b, char c) {
   
  a = a + 1.0;

  if (b == c) 
     return 10;
  else
     return 20;
}

