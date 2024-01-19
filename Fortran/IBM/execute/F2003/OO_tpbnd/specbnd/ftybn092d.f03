!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : chang the accessibility of the
!*                               type-bound procedures by overriding
!*                               it in an extended type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod
      integer :: int = 200
      character*20 :: c = "hi"

      type base
         integer :: x
	 contains
      	 procedure, nopass, private :: bind_b => proc1
      end type

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine
   end module

   module mod1
   use mod
      type, extends(base) :: child
         integer :: y
      contains
         procedure, nopass, public :: bind_b => proc1
      end type
   end module

   use mod1

   type(child) :: dt_p
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3

   call dt_p%bind_b()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   end

