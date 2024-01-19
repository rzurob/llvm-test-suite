! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific binding (inherited binding accessible
!*                               even if the base type is not)
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
    type, private :: base
        contains

        procedure, nopass :: print => printBase
    end type

    type, extends(base) :: child
        integer*4, pointer :: id => null()
    end type

    type (base) :: b1_m
    type (child), save :: c1_m

    contains

    subroutine printBase
        print *, 'base'
    end subroutine printBase

end module

program ftpbnd500
use m, only : child, c1_m, b1_m

    type (child) :: c1
    class (child), allocatable :: c2

    call c1%print

    allocate (c2)

    call c2%print

    call c1_m%print

    call b1_m%print
end
