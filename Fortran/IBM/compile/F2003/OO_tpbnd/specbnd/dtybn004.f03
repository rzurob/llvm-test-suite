!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : private subroutine
!*
!*  SECONDARY FUNCTIONS TESTED : pass
!*
!*  DESCRIPTION                : call the private subroutine in a different
!*                               program unit by using a public
!*                               type bound procedure.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      private proc1
      type parent
         integer :: x
	 contains
      	 procedure, pass :: bind => proc1
      end type

      contains
      subroutine proc1(arg1)
         class(parent) :: arg1
      end subroutine
   end module

   use mod1

   type(parent) :: dt_p
   call dt_p%bind()

   end
