!**********************************************************************
!*  ===================================================================
!*
!*                               ISO_C_BINDING module.
!*
!*  DATE                       : June 9, 2003
!*
!*  DESCRIPTION                : Passing args from C to Fortran.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
#define ARGNAME arg
#define ARGTYPE COMPLEX(c_long_double_complex)
#define VALUE_ATTR , VALUE
#define FPTR_DECL
#define CFPTR_CALL
#define COMPARISON ARGNAME /= (42.0_c_long_double, 84.0_c_long_double)
#include "icc2ff.ft"
