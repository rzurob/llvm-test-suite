#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <complex.h>

void sub(float (**fp)(float *), int *size) {
  *size = sizeof(fp);
}

