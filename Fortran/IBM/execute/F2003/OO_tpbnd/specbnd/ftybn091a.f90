!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftybn091a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn091a.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : parent procedures are inherited.
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
      end type base

      type, extends(base) :: parent
      end type

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine

	end module

   use mod

   type(base) :: dt
   type(parent) :: dt_p
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
   call dt%bind_b()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   int = 0
   c = ""
   call dt_p%bind_b()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   end

