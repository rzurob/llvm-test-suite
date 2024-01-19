!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : pass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : inheritence
!*
!*  DESCRIPTION                : parent procedures are inherited.
!*                               with two levels inheritance.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod
      integer :: int = 200
      character*20 :: c = "hi"

      type parent
         integer :: x
         contains
         procedure, pass :: bind => proc1
      end type

      type, extends(parent) :: child
      end type

      type, extends(child) :: thirdGen
      end type

      contains
      subroutine proc1(arg1)
         class(parent) :: arg1
         int = 400
         c = "hi_again"
      end subroutine

   end module

   use mod

   type(parent) :: dt_p
   type(child) :: dt_c
   type(thirdGen) :: dt_3g
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
   call dt_p%bind()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   int = 0
   c = ""
   call dt_c%bind()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   int = 0
   c = ""
   call dt_3g%bind()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9
   end

