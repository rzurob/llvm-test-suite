!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftybn096j.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn096j.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : testing a procedure is bound to both
!*                                parent and child types,
!*                                but with different binding-names.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod
      integer :: int = 200
      character*20 :: c = "hi"

      type parent
         integer :: x
	 contains
      	 procedure, pass(arg3) :: bind => proc1
         procedure, pass :: bind_r => proc2
      end type

      type, extends(parent) :: child
      contains
         procedure, pass(arg2) :: bind_c => proc1
      end type

      type, extends(child) :: thirGen
      contains
         procedure, pass :: bind_3g => proc1
      end type

      contains
      subroutine proc1(arg1, arg2, arg3)
         class(thirGen) :: arg1
         class(child) :: arg2
         class(parent) :: arg3
         int = 400
         c = "hi_again"
      end subroutine

      subroutine proc2(arg1, arg2, arg3)
         class(parent) :: arg1
         class(child) :: arg2
         class(thirGen) :: arg3
         int = 0
         c = "hello"
      end subroutine
   end module

   use mod

   type(parent) :: dt_p
   type(child) :: dt_c
   type(thirGen) :: dt_3g
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3

   call dt_p%bind(dt_3g, dt_c)
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   call dt_p%bind_r(dt_c, dt_3g)

   call dt_c%bind_c(dt_3g, dt_p)
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7
   call dt_c%bind_r(dt_c, dt_3g)

   call dt_3g%bind_3g(dt_c, dt_p)
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9
   call dt_3g%bind_r(dt_c, dt_3g)
   if (int .ne. 0) error stop 10
   if (c .ne. "hello") error stop 11

   end

