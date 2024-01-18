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
! %POSTCMD: ${TR_SRC}/bdfunc01.sh bdfunc01ba
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : bdfunc01ba.f
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
#define PROGNAME bdfunc01ba
#define VALUE_ATTR , VALUE
#define VALUE_SPECIFIED 1
#include "bdfunc01.ft"
