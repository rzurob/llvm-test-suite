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
! %POSTCMD: dcomp bddiag01r.F
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : bddiag01r.F
!*
!*  DATE                       : July 11, 2003
!*
!*  DESCRIPTION                : Testing the types of components of
!*                               BIND(C) derived types.
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
#define PROGNAME bddiag01r
#define COMPTYPE byte
#define USESTATEMENT
#define DT_DECLARATION
#include "bddiag01.ft"
