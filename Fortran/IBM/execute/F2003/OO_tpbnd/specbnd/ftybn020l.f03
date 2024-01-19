!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : private type bound procedure
!*
!*  SECONDARY FUNCTIONS TESTED : pass, non_overridable
!*
!*  DESCRIPTION                : the accessiblity of a type-bound procedure
!*                               is not affected by a PRIVATE statement
!*                               in the component-part, the accessiblity
!*                               of a data component is not affected by a
!*                               PRIVATE statemnt in the type-bound-procedure
!*                               -part.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type parent
         private
         integer :: x
	 contains
      	 procedure, pass, non_overridable :: bind => proc1
      end type

   type, extends(parent) :: child
   end type

   type dt
      type(child) :: dt_c(3)
   end type

   type(parent) :: dt_p(2)
   type(dt) :: dt_test(3)

   contains
      subroutine proc1(arg1)
         class(parent) :: arg1
         arg1%x = 100
      end subroutine
   subroutine test
      call dt_p(1)%bind()
      call dt_test(1)%dt_c(3)%bind()
      if (dt_p(1)%x .ne. 100)  error stop 2_4
      if (dt_test(1)%dt_c(3)%x .ne. 100)  error stop 3_4
   end subroutine

   end module

   use mod1

   call test

   end
