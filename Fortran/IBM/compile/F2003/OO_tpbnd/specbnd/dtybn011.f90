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
! %POSTCMD: dcomp dtybn011.f
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtybn011.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : binding attributes
!*
!*  SECONDARY FUNCTIONS TESTED : pass
!*
!*  DESCRIPTION                : the same binding attribute shall not
!*                               appear more than once in a given
!*                               binding-attr-list.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type parent
         integer :: x
	 contains
      	 procedure, private, pass, non_overridable, pass, pass, non_overridable :: bind
      end type

      type(parent) :: dt_p

      contains
      subroutine bind(arg1)
         class(parent) :: arg1
      end subroutine

   subroutine test
      call dt_p%bind()
   end subroutine

   end module

   use mod1

   call test

   end

