!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftybn095a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn095a.f
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : type bound procedure
!*
!*  SECONDARY FUNCTIONS TESTED : pass, non_overridable
!*
!*  DESCRIPTION                : testing the derived type with type-bound
!*                               procedure as array element.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type parent
         integer :: x = 100
	 contains
      	 procedure, nopass, non_overridable :: bind => proc1
      end type

   type, extends(parent) :: child
   end type

   type dt
      type(child) :: dt_c(3)
   end type

   contains
      subroutine proc1(arg1)
         class(parent) :: arg1
         arg1%x = 200
      end subroutine

   end module
   use mod1

   type(parent) :: dt_p(2)
   type(dt) :: dt_test(3)

   call dt_p%bind(dt_p(1))
   call dt_test%dt_c(2)%bind(dt_test(1)%dt_c(3))
   if (dt_p(1)%x .ne. 200)  error stop 2_4
   if (dt_p(2)%x .ne. 100)  error stop 22_4
   if (dt_test(1)%dt_c(3)%x .ne. 200)  error stop 3_4
   if (dt_test(2)%dt_c(3)%x .ne. 100)  error stop 4_4
   if (dt_test(3)%dt_c(3)%x .ne. 100)  error stop 5_4
   if (dt_test(1)%dt_c(2)%x .ne. 100)  error stop 6_4
   if (dt_test(2)%dt_c(2)%x .ne. 100)  error stop 7_4
   if (dt_test(3)%dt_c(2)%x .ne. 100)  error stop 8_4
   end
