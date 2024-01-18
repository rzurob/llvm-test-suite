#include <stdio.h>

void sub(int (**g)(int *), int *arg, int *ret) {

   *ret = (*g)(arg);

}
