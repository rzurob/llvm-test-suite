!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp ftybn092e.f ftybn092e.vf
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn092e.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : accessiblity
!*
!*  DESCRIPTION                : chang the accessibility of the
!*                               type-bound procedures by overriding
!*                               it in an extended type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod

      type base
         integer :: x
         contains
      	 procedure, nopass,public :: bind_b => proc1
      end type

      type, extends(base) :: child
         integer :: y
      contains
!* will issure an error message here
         procedure, nopass, private :: bind_b => proc1
      end type

      contains
      subroutine proc1()
      end subroutine

   end module


   end

