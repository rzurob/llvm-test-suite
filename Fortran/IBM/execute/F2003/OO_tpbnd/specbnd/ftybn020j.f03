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
         integer :: x
	 contains
!        private
      	 procedure, pass, non_overridable :: bind => proc1
      end type

      type(parent) :: dt_p

      contains
      subroutine proc1(arg1)
         class(parent) :: arg1
         arg1%x = 100
      end subroutine

   subroutine test1
      call dt_p%bind()
   end subroutine

   end module

   use mod1
   type, extends(parent) :: child
   end type

   type dt
      type(child) :: dt_c
   end type

   type(dt) :: dt_test

   call test1
   call dt_test%dt_c%bind()

   if (dt_p%x .ne. 100)  error stop 2_4
   if (dt_test%dt_c%x .ne. 100)  error stop 3_4

   end