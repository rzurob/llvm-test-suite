#if (!(defined(__linux__) || defined(__APPLE__)))
#pragma options ldbl128
#endif

#include <stdlib.h>


#define true ((unsigned char)1)
#define false ((unsigned char)0)


float crealf(float _Complex arg)
{
  union {
    float _Complex fcomplex;
    struct {
      float real;
      float imag;
    } fstruct;
  } z;
  z.fcomplex = arg;
  return z.fstruct.real;
}

float cimagf(float _Complex arg)
{
  union {
    float _Complex fcomplex;
    struct {
      float real;
      float imag;
    } fstruct;
  } z;
  z.fcomplex = arg;
  return z.fstruct.imag;
}

double creal(long double _Complex arg)
{
  union {
    double _Complex dcomplex;
    struct {
      double real;
      double imag;
    } dstruct;
  } z;
  z.dcomplex = arg;
  return z.dstruct.real;
}

double cimag(double _Complex arg)
{
  union {
    double _Complex dcomplex;
    struct {
      double real;
      double imag;
    } dstruct;
  } z;
  z.dcomplex = arg;
  return z.dstruct.imag;
}

long double creall(long double _Complex arg)
{
  union {
    long double _Complex lcomplex;
    struct {
      long double real;
      long double imag;
    } lstruct;
  } z;
  z.lcomplex = arg;
  return z.lstruct.real;
}

long double cimagl(long double _Complex arg)
{
  union {
    long double _Complex lcomplex;
    struct {
      long double real;
      long double imag;
    } lstruct;
  } z;
  z.lcomplex = arg;
  return z.lstruct.imag;
}


float _Complex createcomplexf(float arg1, float arg2)
{
  union {
    float _Complex fcomplex;
    struct {
      float real;
      float imag;
    } fstruct;
  } z;
  z.fstruct.real = arg1;
  z.fstruct.imag = arg2;
  return z.fcomplex;
}


double _Complex createcomplex(double arg1, double arg2)
{
  union {
    double _Complex dcomplex;
    struct {
      double real;
      double imag;
    } dstruct;
  } z;
  z.dstruct.real = arg1;
  z.dstruct.imag = arg2;
  return z.dcomplex;
}


long double _Complex createcomplexl(long double arg1, long double arg2)
{
  union {
    long double _Complex lcomplex;
    struct {
      long double real;
      long double imag;
    } lstruct;
  } z;
  z.lstruct.real = arg1;
  z.lstruct.imag = arg2;
  return z.lcomplex;
}


struct dt {
  char a;
  char b;
  char c;
  _Bool d;
  char e;
  signed char f;
  char g;
  short h;
  char i;
  int j;
  char k;
  float l;
  char m;
  long long n;
  char o;
  double p;
  char q;
  float _Complex r;
  char s;
  long double t;
  char u;
  double _Complex v;
  char w;
  long double _Complex x;
  char y;
};


void csub(struct dt POINTER_SPEC bcdt)
{
  if ((POINTER_SPEC bcdt).a != 'a') exit(1);
  if ((POINTER_SPEC bcdt).b != 'b') exit(2);
  if ((POINTER_SPEC bcdt).c != 'c') exit(3);
  if ((POINTER_SPEC bcdt).d != false) exit(4);
  if ((POINTER_SPEC bcdt).e != 'e') exit(5);
  if ((POINTER_SPEC bcdt).f != 6) exit(6);
  if ((POINTER_SPEC bcdt).g != 'g') exit(7);
  if ((POINTER_SPEC bcdt).h != 8) exit(8);
  if ((POINTER_SPEC bcdt).i != 'i') exit(9);
  if ((POINTER_SPEC bcdt).j != 10) exit (10);
  if ((POINTER_SPEC bcdt).k != 'k') exit(11);
  if ((POINTER_SPEC bcdt).l != 12.0f) exit(12);
  if ((POINTER_SPEC bcdt).m != 'm') exit(13);
  if ((POINTER_SPEC bcdt).n != 14) exit(14);
  if ((POINTER_SPEC bcdt).o != 'o') exit(15);
  if ((POINTER_SPEC bcdt).p != 16.0) exit(16);
  if ((POINTER_SPEC bcdt).q != 'q') exit(17);
  if ((crealf((POINTER_SPEC bcdt).r) != 18.0f) || 
      (cimagf((POINTER_SPEC bcdt).r) != 36.0f)) exit(18);
  if ((POINTER_SPEC bcdt).s != 's') exit(19);
  if ((POINTER_SPEC bcdt).t != 20.0l) exit(20);
  if ((POINTER_SPEC bcdt).u != 'u') exit(21);
  if ((creal((POINTER_SPEC bcdt).v) != 22.0) ||
      (cimag((POINTER_SPEC bcdt).v) != 44.0)) exit(22);
  if ((POINTER_SPEC bcdt).w != 'w') exit(23);
  if ((creall((POINTER_SPEC bcdt).x) != 24.0l) ||
      (cimagl((POINTER_SPEC bcdt).x) != 48.0l)) exit(24);
  if ((POINTER_SPEC bcdt).y != 'y') exit(25);
  (POINTER_SPEC bcdt).a = 'A';
  (POINTER_SPEC bcdt).b = 'B';
  (POINTER_SPEC bcdt).c = 'C';
  (POINTER_SPEC bcdt).d = true;
  (POINTER_SPEC bcdt).e = 'E';
  (POINTER_SPEC bcdt).f = 60;
  (POINTER_SPEC bcdt).g = 'G';
  (POINTER_SPEC bcdt).h = 80;
  (POINTER_SPEC bcdt).i = 'I';
  (POINTER_SPEC bcdt).j = 100;
  (POINTER_SPEC bcdt).k = 'K';
  (POINTER_SPEC bcdt).l = 120.0f;
  (POINTER_SPEC bcdt).m = 'M';
  (POINTER_SPEC bcdt).n = 140;
  (POINTER_SPEC bcdt).o = 'O';
  (POINTER_SPEC bcdt).p = 160.0;
  (POINTER_SPEC bcdt).q = 'Q';
  (POINTER_SPEC bcdt).r = createcomplexf(180.0f, 360.0f);
  (POINTER_SPEC bcdt).s = 'S';
  (POINTER_SPEC bcdt).t = 200.0l;
  (POINTER_SPEC bcdt).u = 'U';
  (POINTER_SPEC bcdt).v = createcomplex(220.0, 440.0);
  (POINTER_SPEC bcdt).w = 'W';
  (POINTER_SPEC bcdt).x = createcomplexl(240.0l, 480.0l);
  (POINTER_SPEC bcdt).y = 'Y';
}
