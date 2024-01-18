#include <stdlib.h>
#include "cmplx.h"

void sub_arr1(int_least16_t (*a1)[1][2], double (*b1)[1][2][3]);
void sub_arr2(char (*a2)[1][2][3], unsigned char (*b2)[1][2]);
void sub_arr3(float _Complex (*a3)[1]);

int main(int argc, char ** argv)
{   
    int_least16_t a1[1][2];
    double b1[1][2][3];
    char a2[1][2][3];
    unsigned char b2[1][2];
    float _Complex a3[1];
    int i, j, k; 

    /* Testing sub_arr1 */
    for(i=0;i<=0;i++) {
      for(j=0;j<=1;j++) {
	a1[i][j] = i * j;
      }
    }

    for(i=0;i<=0;i++) {
     for(j=0;j<=1;j++) {
      for(k=0;k<=2;k++) {
	b1[i][j][k] = 3.14 ;
      }
     }
    }

    sub_arr1(NULL, NULL); 
    sub_arr1(&a1, NULL); 
    sub_arr1(NULL, &b1); 
    sub_arr1(&a1, &b1);

    /* Testing sub_arr2 */
    for(i=0;i<=0;i++) {
      for(j=0;j<=1;j++) {
        for(k=0;k<=2;k++){
            a2[i][j][k] = 'a';
	}
      }
    }

    for(i=0;i<=0;i++) {
     for(j=0;j<=1;j++) {
        b2[i][j] = i + j;
     }
    }

    sub_arr2(NULL, NULL);
    sub_arr2(&a2, NULL);
    sub_arr2(NULL, &b2);
    sub_arr2(&a2, &b2);

    /* Testing sub_arr3 */
    for(i=0;i<=0;i++) {
        a3[i] = createcomplexf(1.0f, 3.0f);
    }

    sub_arr3(NULL);
    sub_arr3(&a3);

    return 0;
}

