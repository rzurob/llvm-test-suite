!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May 29, 2003
!*
!*  DESCRIPTION                : Passing an argument from Fortran to
!*                               a C function.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YYYY:  Init:  Comments:
!*  05/29/2003   RJ     -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
#define PROGRAM_NAME icf2cfx2
#define ARGTYPE complex(c_float_complex)
#define ARGVALUE (42.0_c_float, 84.0_c_float)
#define VALUE_ATTR
#include "icf2cf.ft"
