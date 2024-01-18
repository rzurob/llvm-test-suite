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
! %POSTCMD: ${TR_SRC}/bdfunc01.sh bdfunc01ag power
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : bdfunc01ag.f
!*
!*  DATE                       : July 11, 2003
!*
!*  DESCRIPTION                : Functional testing of BIND(C) derived
!*                               types.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YYYY:  Init:  Comments:
!*  07/11/2003   RJ     -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
#define PROGNAME bdfunc01ag
#define VALUE_ATTR
#define VALUE_SPECIFIED 0
#include "bdfunc01.ft"
