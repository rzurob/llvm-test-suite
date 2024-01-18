!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp dtybn022.f
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtybn022.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : private type bound procedure
!*
!*  SECONDARY FUNCTIONS TESTED : pass, non_overridable
!*
!*  DESCRIPTION                : testing the non_overridable attribute.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type, private :: parent
         integer :: x
	 contains
      	 procedure, pass, non_overridable :: bind => proc1
      end type

   type, extends(parent) :: child
   end type

   type, extends(child) :: thirGen
      contains
      procedure  :: bind => proc2
   end type

   type dt
      type(child) :: dt_c
   end type

   type(parent) :: dt_p
   type(dt) :: dt_test

   contains
      subroutine proc1(arg1)
         class(parent) :: arg1
         arg1%x = 100
      end subroutine

      subroutine proc2(arg1)
         class(thirGen) :: arg1
         arg1%x = 100
      end subroutine

   subroutine test
      call dt_p%bind()
      call dt_test%dt_c%bind()
      if (dt_p%x .ne. 100)  error stop 2_4
      if (dt_test%dt_c%x .ne. 100)  error stop 3_4
   end subroutine

   end module

!   use mod1

!   call test

   end
