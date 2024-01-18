#include <stdlib.h>
#include <stdint.h>

struct st0 {
  intmax_t a;
};

struct st1 {
  struct st0 s0;
  int  i[2];
};

struct st2 {
  struct st1 s1;
};

void sub(void * a);

int main(int argc, char ** argv)
{   

    struct st0 my_st0;
    struct st1 my_st1;
    struct st2 my_st2;

    sub(&my_st0);
    sub(&my_st1);
    sub(&my_st2);

    return 0;
}
