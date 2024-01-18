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
! %POSTCMD: dcomp ftybn093d.f ftybn093d.vf
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn093d.f
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
      	 procedure, pass :: bind_b => proc1
      end type

      type, extends(base) :: child
      contains
         procedure, pass :: bind_b => proc2
      end type

      contains
      function proc1(arg1) result(arg2)
          class(base), intent(in) :: arg1
          integer :: arg2
          arg2 = 100
      end function

      function proc2(arg1) result(arg2)
          class(child), intent(in) :: arg1
          character*20 :: arg2
          arg2 = "hello"
      end function

   end module

   end

