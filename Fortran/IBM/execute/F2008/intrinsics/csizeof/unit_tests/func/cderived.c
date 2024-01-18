#include <stdint.h>

struct my_type1
{
  short s[2];
};

struct my_type2
{
  float r1;
  long double r2;
  double r3;
};

struct my_type3
{
  char str2;
  short sint1[3];
  double _Complex c1;
  int8_t bint;
  uint8_t bool;
};

struct my_type4
{
  uint8_t bool;
  struct my_type3 t_f_34;
};

int get_sizeof_my_type1()
{
  return sizeof(struct my_type1);
}

int get_sizeof_my_type2()
{
  return sizeof(struct my_type2);
}

int get_sizeof_my_type3()
{
  return sizeof(struct my_type3);
}

int get_sizeof_my_type4()
{
  return sizeof(struct my_type4);
}
