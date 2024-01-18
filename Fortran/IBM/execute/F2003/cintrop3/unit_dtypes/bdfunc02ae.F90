!#######################################################################
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qalign=bindc=natural
! %GROUP: bdfunc02ae.F
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : bdfunc02ae.F
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
#define PROGNAME bdfunc02ae
#define MODNAME mod_bdfunc02ae
#define VALUE_ATTR
#define VALUE_SPECIFIED 0
#include "bdfunc02.ft"
