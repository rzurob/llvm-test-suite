!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftybn091g.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn091g.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : parent procedures are inherited.
!*                               with two levels inheritance.
!*                               inherite from a different scoping units.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

	module mod
      integer :: int = 200
      character*20 :: c = "hi"

		type base
         integer :: x
		contains
      	procedure, nopass :: bind_b => proc1
         procedure, nopass :: bind_r => proc2
		end type base

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine

      subroutine proc2()
         int = 0
         c = ""
      end subroutine
	end module

   module mod1
   use mod
   type, extends(base) :: parent
   end type
   end module

   module mod2
   use mod1
   type, extends(parent) :: child
   end type
   end module

   use mod2

   type(base) :: dt
   type(parent) :: dt_p
   type(child) :: dt_c
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
   call dt%bind_b()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   call dt_p%bind_r()
   call dt_p%bind_b()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7
   call dt_c%bind_r()
   call dt_c%bind_b()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9
   end

