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
! %POSTCMD: dcomp bddiag03.f
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : bddiag03.f
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
      program bddiag03
        type, bind(c) :: dt
          character(*) :: a
        end type
      end
