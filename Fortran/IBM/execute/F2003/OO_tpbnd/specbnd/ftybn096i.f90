!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftybn096i.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn096i.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : pass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : inheritance
!*
!*  DESCRIPTION                : testing a procedure is bound to different
!*                               types.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod
      integer :: int = 200
      character*20 :: c = "hi"

      type base1
         integer :: x
	 contains
      	 procedure, pass(arg1) :: bind_b1 => proc1
      end type

      type base2
         integer :: x
      contains
         procedure, pass(arg2) :: bind_b2 => proc1
      end type

      type base3
         integer :: x
      contains
         procedure, pass(arg3) :: bind_b3 => proc1
      end type

      contains
      subroutine proc1(arg1, arg2, arg3)
         class(base1) :: arg1
         class(base2) :: arg2
         class(base3) :: arg3
         int = 400
         c = "hi_again"
      end subroutine

	end module

   use mod

   type(base1) :: dt1
   type(base2) :: dt2
   type(base3) :: dt3
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
   call dt1%bind_b1(dt2, dt3)
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   int = 0
   c = ""
   call dt2%bind_b2(dt1, dt3)
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7
   int = 0
   c = ""
   call dt3%bind_b3(dt1, dt2)
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   end

