!#######################################################################
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qalign=bindc=full
! %GROUP: bdfunc02bc.F
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : bdfunc02bc.F
!*
!*  DATE                       : July 23, 2003
!*
!*  DESCRIPTION                : Functional testing of derived types
!*                               with the BIND(C) attribute.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YYYY:  Init:  Comments:
!*  07/23/2003   RJ     -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
#define PROGNAME bdfunc02bc
#define MODNAME mod_bdfunc02bc
#define VALUE_ATTR , VALUE
#define VALUE_SPECIFIED 1
#include "bdfunc02.ft"
