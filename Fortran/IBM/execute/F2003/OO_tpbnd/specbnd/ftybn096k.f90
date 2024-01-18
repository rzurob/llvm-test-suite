!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftybn096k.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn096k.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : pass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : inheritance
!*
!*  DESCRIPTION                : testing the base procedure is bound to
!*                               two types which all extend the base type,
!*                               with different binding-names.
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

      type, extends(parent) :: child1
         contains
         procedure, pass(arg2) :: bind_c1 => proc1
      end type

      type, extends(parent) :: child2
         contains
         procedure, pass(arg3) :: bind_c2 => proc1
      end type

      contains
      subroutine proc1(arg1, arg2, arg3)
         class(parent) :: arg1
         class(child1) :: arg2
         class(child2) :: arg3
         int = 400
         c = "hi_again"
      end subroutine

      subroutine proc2()
         int = 0
         c = ""
      end subroutine
   end module

   use mod

   type(parent) :: dt_p
   type(child1) :: dt_c1
   type(child2) :: dt_c2
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
   call dt_p%bind(dt_c1, dt_c2)
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5

   call proc2()
   call dt_c1%bind_c1(dt_p, dt_c2)
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   call proc2()
   call dt_c2%bind_c2(dt_p, dt_c1)
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9

   end

