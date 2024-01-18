!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftybn092c.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn092c.f
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

      integer :: int = 200
      character*20 :: c = "hi"

      type base
         integer :: x
	 contains
      	 procedure, nopass, private :: bind_b => proc1
      end type

      type, extends(base) :: parent
      contains
      	 procedure, nopass :: bind_b => proc1
      end type

      type, extends(parent) :: child
      contains
         procedure, nopass, public :: bind_b => proc1
      end type

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine

   end module

   use mod

   type(child) :: dt_c
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3

   call dt_c%bind_b()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9

   end

