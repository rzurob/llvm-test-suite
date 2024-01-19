!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : private derived type
!*
!*  SECONDARY FUNCTIONS TESTED : nopass
!*
!*  DESCRIPTION                : testing the type bound procedure
!*                               which within a private derived type
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type,private :: parent
         integer :: x
	 contains
      	 procedure, nopass :: bind => proc1
      end type

   type(parent) :: dt_p

      contains
      subroutine proc1(arg1)
         class(parent) :: arg1
         arg1%x = 200
      end subroutine

   end module

   use mod1
   call dt_p%bind(dt_p)

   if ( dt_p%x .ne. 200) error stop 2_4

   end

