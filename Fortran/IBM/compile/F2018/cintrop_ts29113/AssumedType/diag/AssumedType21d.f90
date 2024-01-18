!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 13, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Assumed type dummy argument cannot be argument
!*                               associated with an actual argument that is of
!*                               derived type if it has type parameters
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module  mod
  implicit none

  type dt (k1,l1)
    integer, kind :: k1 = 2
    integer, len  :: l1 = 5

    integer(kind=k1) :: my_arr(l1)
  end type dt

  contains
  subroutine module_sub(c)
     type(*) :: c

  end subroutine module_sub
end module mod
program AssumedType21d
use mod
implicit none

type(dt(4,10)) :: dt0
type(dt) :: dt1

call sub(dt0)
call sub(dt1)
call module_sub(dt0)
call module_sub(dt1)

contains

   subroutine sub(a)
      type(*) :: a

   end subroutine sub
end program AssumedType21d
