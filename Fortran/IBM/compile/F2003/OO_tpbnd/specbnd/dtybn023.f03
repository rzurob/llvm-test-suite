!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : type bound procedure
!*
!*  SECONDARY FUNCTIONS TESTED : nopass
!*
!*  DESCRIPTION                : If the interface of the binding has no
!*                               dummy argument of the type being
!*                               defined, NOPASS shall appear.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type  parent
         integer :: x
	 contains
      	 procedure :: bind => proc1
      end type

   contains
      subroutine proc1()
      end subroutine

   end module

!   use mod1

   end