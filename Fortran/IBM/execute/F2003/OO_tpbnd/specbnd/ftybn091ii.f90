!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : testing a procedure is bounded to different t
!*                               ypes withing different scopes.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod1
      integer :: int = 200
      character*20 :: c = "hi"

      type base1
         integer :: x
      contains
      	 procedure, nopass :: bind_b1 => proc1
      end type

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine

	end module

   module mod2
      use mod1
      type base2
         integer :: x
      contains
         procedure, nopass :: bind_b2 => proc1
      end type
   end module

   module mod3
      use mod2
      type base3
         integer :: x
      contains
         procedure, nopass :: bind_b3 => proc1
      end type
   end module

   use mod3

   type(base1) :: dt1
   type(base2) :: dt2
   type(base3) :: dt3
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
   call dt1%bind_b1()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   int = 0
   c = ""
   call dt2%bind_b2()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7
   int = 0
   c = ""
   call dt3%bind_b3()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   end

