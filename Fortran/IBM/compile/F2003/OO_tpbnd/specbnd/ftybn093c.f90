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
! %POSTCMD: dcomp ftybn093c.f ftybn093c.vf
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn093c.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : The overriding binding and the overriden
!*                               binding shall satisfy the following
!*                               condition: both shall be functions having
!*                               the same result characteristics.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod
      type base
         integer :: x
	 contains
      	 procedure, nopass :: bind_b => proc1
      end type

      type, extends(base) :: parent1
      contains
         procedure, nopass :: bind_b => proc2
      end type

      type, extends(base) :: parent2
      contains
         procedure, nopass  :: bind_b => proc3
      end type

      contains
      function proc1() result(arg1)
         integer :: arg1
         arg1 = 100
      end function

      function proc2() result(arg1)
         character*20 :: arg1
         arg1 = "hello"
      end function

      subroutine proc3()
      end subroutine

   end module

   end

