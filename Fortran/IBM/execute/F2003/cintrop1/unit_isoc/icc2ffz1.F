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
! %POSTCMD: ${TR_SRC}/icc2f.sh z1
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Interlanguage calls
!*                               ISO_C_BINDING module.
!*
!*  DATE                       : June 9, 2003
!*
!*  DESCRIPTION                : Passing args from C to Fortran.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YYYY:  Init:  Comments:
!*  06/09/2003   RJ     -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
      @PROCESS FREE(F90)
#define ARGNAME arg
#define ARGTYPE COMPLEX(c_long_double_complex)
#define VALUE_ATTR , VALUE
#define FPTR_DECL
#define CFPTR_CALL
#define COMPARISON ARGNAME /= (42.0_c_long_double, 84.0_c_long_double)
#include "icc2ff.ft"
