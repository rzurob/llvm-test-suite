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
! %POSTCMD: dcomp ftybn093aa.f
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn093a.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : The overriding binding and the overriden
!*                               binding shall satisfy the following
!*                               condition: both shall be subroutines
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod

      type base
         integer :: x
      contains
      	 procedure, nopass :: bind_b => proc1
      end type base

      type, extends(base) :: parent
      end type

      type, extends(parent) :: child
      contains
!* expect the error massage 1514-631
         procedure, nopass  :: bind_b => proc3
      end type

      contains
      subroutine proc1()
      end subroutine

      integer function proc3()
         proc3 = 100
      end function

   end module

   end

