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
! %POSTCMD: dcomp bddiag12.f
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : bddiag12.f
!*
!*  DATE                       : July 11, 2003
!*
!*  DESCRIPTION                : Testing pointers inside of
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
      program bddiag12
        type, bind(c) :: dt
          character, allocatable :: a
        end type
      end
