!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : accessibility
!*
!*  DESCRIPTION                : testing the accessiblity of parent's
!*                               type-bound procedures are overrided by
!*                               the multiple generations extended types.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod

      type base
         integer :: x
	 contains
      	 procedure, nopass :: bind_b => proc1
      end type

      type, extends(base) :: parent
		contains
!* no error message here
      	procedure, nopass, public :: bind_b => proc1
      end type

      type, extends(parent) :: child
      contains
!* expected an error message here
         procedure, nopass, private :: bind_b => proc1
      end type

      contains
      subroutine proc1()
      end subroutine

   end module


   end

