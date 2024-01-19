!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : binding attributes
!*
!*  SECONDARY FUNCTIONS TESTED : pass
!*
!*  DESCRIPTION                : if =>procedure-name appears, the
!*                               double-colon separator shall
!*                               appear.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type parent
         integer :: x
	 contains
      	 procedure  bind => proc
      end type

      type(parent) :: dt_p

      contains
      subroutine proc(arg1)
         class(parent) :: arg1
      end subroutine

   end module

!   use mod1

   end

