!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : private type bound procedure
!*
!*  SECONDARY FUNCTIONS TESTED : pass
!*
!*  DESCRIPTION                : testing type bound procedure with
!*                               private attribute.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type parent
         integer :: x
	 contains
      	 procedure,private, pass :: bind => proc1
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
   call test1
   if (dt_p%x .ne. 100)   error stop 2_4

   end

