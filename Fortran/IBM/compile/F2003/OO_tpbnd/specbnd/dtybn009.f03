!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : procedure name
!*
!*  SECONDARY FUNCTIONS TESTED : pass
!*
!*  DESCRIPTION                : if neither =>procedure-name nor interface-name
!*                               appears, it is as though =>procedure-name
!*                               had appeared with a procedure name the same
!*                               as the binding name.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type parent
         integer :: x
	 contains
      	 procedure, private, pass :: bind
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
