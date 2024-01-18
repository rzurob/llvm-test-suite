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
! %POSTCMD: dcomp bddiag01u.F
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : bddiag01u.F
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
#define PROGNAME bddiag01u
#define COMPTYPE type(bcdt)
#define USESTATEMENT
#define DT_DECLARATION type, bind(c) :: bcdt; integer(4) a; end type
#include "bddiag01.ft"
