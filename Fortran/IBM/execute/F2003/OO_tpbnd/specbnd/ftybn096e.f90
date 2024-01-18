!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftybn096e.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn096e.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : pass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : inheritance
!*
!*  DESCRIPTION                : parent procedures are inherited.
!*                               inherite from a different scroping unit.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      integer :: int = 200
      character*20 :: c = "hi"

      type parent
         integer :: x
	 contains
      	 procedure, pass :: bind => proc1
      end type

      contains
      subroutine proc1(arg1)
         class(parent) :: arg1
         int = 400
         c = "hi_again"
      end subroutine
   end module

   module mod2
   use mod1
   type, extends(parent) :: child
      integer :: y
   end type
   end module

   use mod2

   type(parent) :: dt_p
   type(child) :: dt_c
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

   end

