/*
 * The functions in this file are intentionally designed to be insensitive to
 * complex argument linkage.
 */

#include <math.h>
#include <complex.h>

typedef float _Complex c4;
typedef double _Complex c8;
typedef long double _Complex c16;

/* start complex(4) */

void c_fc_acos(const c4 * const x, c4 *res)
{
  *res = cacosf(*x);
}

void c_fc_asin(const c4 * const x, c4 *res)
{
  *res = casinf(*x);
}

void c_fc_atan(const c4 * const x, c4 *res)
{
  *res = catanf(*x);
}

void c_fc_acosh(const c4 * const x, c4 *res)
{
  *res = cacoshf(*x);
}

void c_fc_asinh(const c4 * const x, c4 *res)
{
  *res = casinhf(*x);
}

void c_fc_atanh(const c4 * const x, c4 *res)
{
  *res = catanhf(*x);
}

void c_fc_cosh(const c4 * const x, c4 *res)
{
  *res = ccoshf(*x);
}

void c_fc_sinh(const c4 * const x, c4 *res)
{
  *res = csinhf(*x);
}

void c_fc_tanh(const c4 * const x, c4 *res)
{
  *res = ctanhf(*x);
}

void c_fc_tan(const c4 * const x, c4 *res)
{
  *res = ctanf(*x);
}

/* end complex(4) */

/* start complex(8) */

void c_dc_acos(c8 *x, c8 *res)
{
  *res = cacos(*x);
}

void c_dc_asin(const c8 * const x, c8 *res)
{
  *res = casin(*x);
}

void c_dc_atan(const c8 * const x, c8 *res)
{
  *res = catan(*x);
}

void c_dc_acosh(const c8 * const x, c8 *res)
{
  *res = cacosh(*x);
}

void c_dc_asinh(const c8 * const x, c8 *res)
{
  *res = casinh(*x);
}

void c_dc_atanh(const c8 * const x, c8 *res)
{
  *res = catanh(*x);
}

void c_dc_cosh(const c8 * const x, c8 *res)
{
  *res = ccosh(*x);
}

void c_dc_sinh(const c8 * const x, c8 *res)
{
  *res = csinh(*x);
}

void c_dc_tanh(const c8 * const x, c8 *res)
{
  *res = ctanh(*x);
}

void c_dc_tan(const c8 * const x, c8 *res)
{
  *res = ctan(*x);
}

/* end complex(8) */

#if __linux__
/* start complex(16) */

#if __IBMC__
#define CONV_TO_GCC_C16(x) (__ibm2gccldbl_cmplx(x))
#else
#define CONV_TO_GCC_C16(x) (x)
#endif

void c_ldc_acos(const c16 * const x, c16 *res)
{
  *res = cacosl(CONV_TO_GCC_C16(*x));
}

void c_ldc_asin(const c16 * const x, c16 *res)
{
  *res = casinl(CONV_TO_GCC_C16(*x));
}

void c_ldc_atan(const c16 * const x, c16 *res)
{
  *res = catanl(CONV_TO_GCC_C16(*x));
}

void c_ldc_acosh(const c16 * const x, c16 *res)
{
  *res = cacoshl(CONV_TO_GCC_C16(*x));
}

void c_ldc_asinh(const c16 * const x, c16 *res)
{
  *res = casinhl(CONV_TO_GCC_C16(*x));
}

void c_ldc_atanh(const c16 * const x, c16 *res)
{
  *res = catanhl(CONV_TO_GCC_C16(*x));
}

void c_ldc_cosh(const c16 * const x, c16 *res)
{
  *res = ccoshl(CONV_TO_GCC_C16(*x));
}

void c_ldc_sinh(const c16 * const x, c16 *res)
{
  *res = csinhl(CONV_TO_GCC_C16(*x));
}

void c_ldc_tanh(const c16 * const x, c16 *res)
{
  *res = ctanhl(CONV_TO_GCC_C16(*x));
}

void c_ldc_tan(const c16 * const x, c16 *res)
{
  *res = ctanl(CONV_TO_GCC_C16(*x));
}
#endif

/* end complex(16) */

