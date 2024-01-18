!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 13, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Assumed type dummy argument cannot be argument
!*                               associated with an actual argument that is of
!*                               derived type if it has a final subroutine
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module  mod
  implicit none

  type dt
    integer :: cmp

    contains
     final :: final_sub
  end type dt

  contains
  subroutine final_sub(a)
    type(dt), intent(in) :: a

  end subroutine final_sub

  subroutine module_sub(a)
     type(*) :: a

  end subroutine module_sub
end module mod
program AssumedType23d
use mod
implicit none

type(dt) :: dt0

call sub(dt0)
call module_sub(dt0)

contains

   subroutine sub(a)
      type(*) :: a

   end subroutine sub
end program AssumedType23d
