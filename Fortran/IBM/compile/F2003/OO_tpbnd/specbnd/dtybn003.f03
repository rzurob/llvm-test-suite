!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : private type bound procedure
!*
!*  SECONDARY FUNCTIONS TESTED : non_overridable
!*
!*  DESCRIPTION                : a binding-private-stmt is permitted only
!*                               if the type definition is within the
!*                               specification part of a module.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type parent
         integer :: x
	 contains
      	 procedure,private, non_overridable :: bind => proc1
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
