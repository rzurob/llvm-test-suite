! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-07-19
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
module m
  type X
    real r,s,t
  end type

  contains

  subroutine foo (r)
    real r(10)[*]

    r = 1.0
  end subroutine

  subroutine bar (x, n)
    integer, intent(in) :: n
    real, intent(inout) :: x(*)[*]

    x(:n) = 1.0
  end subroutine
end module

use m
    type(X), save :: x1(10)[*]

    call foo (x1%r) !<-- illegal, let's prevent it

    call bar (x1%t, 10) !<-- illegal
    call bar (x1%s, 10) !<-- illegal
end

