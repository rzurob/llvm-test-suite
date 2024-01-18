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
! %POSTCMD: ${TR_SRC}/icf2c.sh 12
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : icf2cf12.F
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
#define PROGRAM_NAME icf2cf12
#define ARGTYPE character(kind = c_char)
#define ARGVALUE 'a'
#define VALUE_ATTR
#include "icf2cf.ft"
