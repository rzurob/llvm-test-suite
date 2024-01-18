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
! %POSTCMD: dcomp icdiag04p.F
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : icdiag04p.F
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
#define PROGNAME icdiag04p
#define DT_DECL
#define TYPENAME double complex
#include "icdiag04.ft"
