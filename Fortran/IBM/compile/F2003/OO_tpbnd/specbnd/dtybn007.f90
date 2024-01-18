!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : private subroutine
!*
!*  SECONDARY FUNCTIONS TESTED : pass
!*
!*  DESCRIPTION                : testing the private type bound
!*                               procedure within a private derived
!*                               type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type,private :: parent
         integer :: x
	 contains
      	 procedure, private, pass :: bind => proc1
      end type

   type(parent) :: dt_p

      contains
      subroutine proc1(arg1)
         class(parent) :: arg1
      end subroutine

   end module

   use mod1
   call dt_p%bind()

   end

