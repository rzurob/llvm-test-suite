#include <stdio.h>
#include <stdint.h>

struct dt0 {
  intmax_t a;
};

struct dt1 {
  struct dt0 d0;
  intptr_t a[2]; 
};

struct dt2 {
  struct dt1 d1;
  long long a[2];
};


int cfun(int (*fproc)())
{   
    struct dt0 dta;
    struct dt1 dtb[2][2];  
    struct dt2 dtc[2][2];  
    int i, j, k, res=0; 

    dta.a = 50;

    for(i=0;i<2;i++) {
      for(j=0;j<2;j++) {
        dtb[i][j].d0.a = i + j + 1000;
        for(k=0;k<2;k++) {
          dtb[i][j].a[k] = i + j + k;
        }
      }
    }

    for(i=0;i<2;i++) {
      for(j=0;j<2;j++) {
          dtc[i][j].d1.d0.a = i + j + 1000;
        for(k=0;k<2;k++) {
          dtc[i][j].a[k] = i + j + k + 3;
          dtc[i][j].d1.a[k] = dtc[i][j].a[k];
        }
      }
    }

    /* Testing 1 */

    res = fproc(dta, NULL, &dtc);   

    /* verify */
    if (res != 10) {
	printf ("res is expected 10, actual is %d\n", res );
        return 1; 
    }

    if (dta.a != 50) {
	printf ("dta.a is expected 50, actual is %d\n", dta.a);
        return 1;
    }

    for(i=0;i<2;i++) {
      for(j=0;j<2;j++) {
        for(k=0;k<2;k++) {
          if (dtc[i][j].a[k] != i + j + k + 4) {
              printf("after 1st call : dtc[%d][%d] is expected %d, actual is %d\n", i, j, i + j + k + 4, dtc[i][j].a[k]);
              return 1;
          }
        }
      }
    }

    /* Testing 2 */

    for(i=0;i<2;i++) {
      for(j=0;j<2;j++) {
        for(k=0;k<2;k++) {
          dtc[i][j].a[k] = i + j + k + 3;
        }
      }
    }

    res = fproc(dta, &dtb, &dtc);
   
    /* verify */

    if (res != 100) {
        printf ("res is expected 100, actual is %d\n", res );
        return 1;
    }

    if (dta.a != 50) {
            printf ("dta.a is expected 50, actual is %d\n", dta.a);
            return 1;
    }

    for(i=0;i<2;i++) {
      for(j=0;j<2;j++) {
        for(k=0;k<2;k++) {
          if (dtc[i][j].a[k] != i + j + k + 4) {
              printf("after 2nd call : dtc[%d][%d] is expected %d, actual is %d\n", i, j, i + j + k + 4, dtc[i][j].a[k]);
              return 1;
          }
        }
      }
    }

    return 0;
}

