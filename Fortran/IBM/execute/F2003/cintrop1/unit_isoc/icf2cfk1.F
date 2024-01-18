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
! %POSTCMD: ${TR_SRC}/icf2c.sh k1
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : icf2cfk1.F
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
#define PROGRAM_NAME icf2cfk1
#define ARGTYPE integer(c_int64_t)
#define ARGVALUE 42
#define VALUE_ATTR , VALUE
#include "icf2cf.ft"
