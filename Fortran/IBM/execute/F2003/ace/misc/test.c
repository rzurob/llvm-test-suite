#include <stdio.h>
#include <limits.h>
#include <float.h>
#include <complex.h>
#include <stdbool.h>
#include <math.h>

/* Receives array data from FORTRAN and verifies that it is as expected.
 */


#define MAX_ITEMS 32

signed char signed_char_record[MAX_ITEMS];
short int short_int_record[MAX_ITEMS];
int int_record [MAX_ITEMS];
long int  long_int_record[MAX_ITEMS];
long long int  long_long_int_record[MAX_ITEMS];

float  float_record[MAX_ITEMS];
double  double_record[MAX_ITEMS];
long double  long_double_record[MAX_ITEMS];

float _Complex  complex_float_record[MAX_ITEMS];
double _Complex  complex_double_record[MAX_ITEMS];
long double _Complex complex_long_double_record[MAX_ITEMS];

char char_record [MAX_ITEMS];
_Bool bool_record [MAX_ITEMS];



void record_ints(int len, int incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    int_record [inx] = incoming[inx];
}

int get_int(int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? int_record [inx-1] : 0;
}

void record_short_ints(int len, short int incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    short_int_record [inx] = incoming[inx];
}

short int get_short_int(int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? short_int_record [inx-1] : 0;
}


void record_long_ints(int len, long int incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    long_int_record [inx] = incoming[inx];
}

long int get_long_int(int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? long_int_record [inx-1] : 0;
}


void record_long_long_ints(int len, long long int incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    long_long_int_record [inx] = incoming[inx];
}

long long int get_long_long_int(int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? long_long_int_record [inx-1] : 0;
}


void record_signed_chars(int len, signed char incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    signed_char_record [inx] = incoming[inx];
}

signed char get_signed_char(int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? signed_char_record [inx-1] : 0;
}




void record_floats(int len, float incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    float_record [inx] = incoming[inx];
}

float get_float(int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? float_record [inx-1] : 0;
}


void record_doubles(int len, double incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    double_record [inx] = incoming[inx];
}

double get_double(int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? double_record [inx-1] : 0;
}


void record_long_doubles(int len, long double incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    long_double_record [inx] = incoming[inx];
}

long double get_long_double(int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? long_double_record [inx-1] : 0;
}




void record_complex_floats(int len, float _Complex incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    complex_float_record [inx] = incoming[inx];
}

float _Complex get_complex_float(int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? complex_float_record [inx-1] : 0;
}


void record_complex_doubles(int len, double _Complex incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    complex_double_record [inx] = incoming[inx];
}

double _Complex get_complex_double(int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? complex_double_record [inx-1] : 0;
}


void record_complex_long_doubles(int len, long double _Complex incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    complex_long_double_record [inx] = incoming[inx];
}

long double _Complex get_complex_long_double(int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? complex_long_double_record [inx-1] : 0;
}




void record_chars(int len, char incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    char_record [inx] = incoming[inx];
}

char get_char(int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? char_record [inx-1] : 0;
}


void record_bools(int len, _Bool incoming[]) {
  int inx;
  if (len > MAX_ITEMS)
    len = MAX_ITEMS;
  for (inx = 0; inx < len; ++inx)
    bool_record [inx] = incoming[inx];
}

_Bool get_bool(int inx) {
  return (inx > 0 && inx <= MAX_ITEMS) ? bool_record[inx-1] : 0;
}
