!#######################################################################
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qalign=bindc=packed
! %GROUP: bdfunc02bf.F
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : bdfunc02bf.F
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
#define PROGNAME bdfunc02bf
#define MODNAME mod_bdfunc02bf
#define VALUE_ATTR , VALUE
#define VALUE_SPECIFIED 1
#include "bdfunc02.ft"
