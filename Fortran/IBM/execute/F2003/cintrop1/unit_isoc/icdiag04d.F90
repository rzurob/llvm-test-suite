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
! %POSTCMD: dcomp icdiag04d.F
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : icdiag04d.F
!*                               ISO_C_BINDING module.
!*
!*  DATE                       : June 17, 2003
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YYYY:  Init:  Comments:
!*  06/17/2003   RJ     -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
#define PROGNAME icdiag04d
#define DT_DECL type :: t; integer(4) i; end type
#define TYPENAME type(t)
#include "icdiag04.ft"
