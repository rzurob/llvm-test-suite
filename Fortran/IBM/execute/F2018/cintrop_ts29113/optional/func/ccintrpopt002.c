#include <stdlib.h>
#include "cmplx.h"

int main(int argc, char ** argv)
{   
    void sub_testf(int *, short int *, long int *);
    int func_testf(long double *, double *, float *);

    int             i1_arr[5][5], rc, i, j, k, l;
    short int       i2_arr[5][5][5];
    long int        i3_arr[5][5][5][5];

    long double     r1_arr[5][5][5];
    double          r2_arr[5][5], r2_arr2[5][5];
    float           r3_arr[5];
    double          EPSLION_D=0.00001;
    float           EPSLION_F=0.00001;

    /* Test 1: using NULL as actual argument as actual argument */
    sub_testf(NULL, NULL, NULL);
    rc = func_testf(NULL, NULL, NULL);
 
    /* verify */
    if (rc != 0) {
	printf ("rc : expected 0, actual %d \n", rc);
        exit (1);
    }

    /* Test 2: mixed NULL and corresponding C types as actual argument */

    /* initialization for Test 2 */
    for (i=0; i<5; i++) {
      for (j=0; j<5; j++) {
    	i1_arr[i][j] = 1;
        for (k=0; k<5; k++) {
          for (l=0; l<5; l++) {
            i3_arr[i][j][k][l]=2;   
          }
        }
      }
    }

    sub_testf(&i1_arr[0][0], NULL, &i3_arr[0][0][0][0]);
    rc = func_testf(NULL, &r2_arr[0][0], NULL);

    /* verify for Test 2 */
    if (rc != 1) {
        printf ("rc : expected 1, actual %d \n", rc);
        exit (2);
    }

    for (i=0; i<5; i++) {
      for (j=0; j<5; j++) {
        for (k=0; k<5; k++) {
          for (l=0; l<5; l++) {
            if (i3_arr[i][j][k][l] != 0) {
		printf ("Test 2 i3_arr[%d][%d][%d][%d] : expected 0, actual %d\n", i, j, k, l, i3_arr[i][j][k][l]);
 		exit (3);
	    }
          }
        }
      }
    }

    for (i=0; i<5; i++) {
      for (j=0; j<5; j++) {
            if (! compDouble(r2_arr[i][j], (i+1)*(j+1)*1.0, EPSLION_D)) {
                printf ("r2_arr[%d][%d] : expected %lf, actual %lf\n", i, j, (i+1)*(j+1)*1.0, r2_arr[i][j]);
                exit (4);
            }
      }
    }


    /* Test 3: mixed NULL and corresponding C types as actual argument */

    /* initialization for Test 2 */
    for (i=0; i<5; i++) {
      for (j=0; j<5; j++) {
        i1_arr[i][j] = 3;
        for (k=0; k<5; k++) {
            i2_arr[i][j][k]=3;
        }
      }
    }

    sub_testf(&i1_arr[0][0], &i2_arr[0][0][0], NULL);
    rc = func_testf(NULL, &r2_arr2[0][0], r3_arr);

    /* verify for Test 3 */
    if (rc != 2) {
        printf ("rc : expected 2, actual %d \n", rc);
        exit (5);
    }

    for (i=0; i<5; i++) {
      for (j=0; j<5; j++) {
            if (! compDouble(r2_arr2[i][j], (i+1)*(j+1)*1.0, EPSLION_D)) {
                printf ("r2_arr2[%d][%d] : expected %lf, actual %lf\n", i, j, (i+1)*(j+1)*1.0, r2_arr2[i][j]);
                exit (6);
            }
      }
    }

    for (i=0; i<5; i++) {
            if (! compFloat(r3_arr[i], 2.0, EPSLION_F)) {
                printf ("r3_arr[%d] : expected 2.0, actual %f\n", i, r3_arr[i]);
                exit (7);
            }
    }

    return 0;
}
