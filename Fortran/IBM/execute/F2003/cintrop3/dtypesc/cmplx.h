
#include <stdio.h>

float crealf(float _Complex);
float cimagf(float _Complex);
float _Complex createcomplexf(float, float);
double creal(double _Complex);
double cimag(double _Complex);
double _Complex createcomplex(double, double);
long double creall(long double _Complex);
long double cimagl(long double _Complex);
long double _Complex createcomplexl(long double, long double);

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

double creal(double _Complex arg)
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


