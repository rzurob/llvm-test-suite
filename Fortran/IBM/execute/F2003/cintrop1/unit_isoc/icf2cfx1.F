!#######################################################################
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: ${TR_SRC}/icf2c.sh x1
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : icf2cfx1.F
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
#define PROGRAM_NAME icf2cfx1
#define ARGTYPE complex(c_float_complex)
#define ARGVALUE (42.0_c_float, 84.0_c_float)
#define VALUE_ATTR , VALUE
#include "icf2cf.ft"
