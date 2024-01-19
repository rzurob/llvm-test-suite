! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (module procedure as
!                               the type bound; the accessibility is obtained
!                               though use association; test case has very
!                               little to do with the pointer assignment)
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
    contains

    subroutine testSum (a, b)
        integer*4, intent(in) :: a, b

        if (a+b > 10) then
            print *, 'large'
        else
            print *, 'small'
        end if
    end subroutine
end module

program fpAssgn018a
use m

    type base
        integer*4 :: value = 10

        contains

        procedure, nopass :: testAdd => testSum
    end type

    type, extends (base) :: child
        character(20) :: name
    end type

    class (base), pointer :: b1, b2
    type (child), target :: c1, c2

    c1%value = 1

    call c1%testAdd (c1%value, c2%value)

    call c1%testAdd (0, c2%value)

    b1 => c2

    call abc (c1, b1)

    allocate (b1)
    b1%value = 5

    b2 => b1
    call abc (b1, b2)

    deallocate (b2)

    contains

    subroutine abc (b1, b2)
        class (base), intent(in) :: b1, b2

        call b1%testAdd (b1%value, b2%value)
    end subroutine
end
