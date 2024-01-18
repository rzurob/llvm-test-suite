! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (data-target is the
!                               dummy-arg)
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
    type base
        integer*4 :: id = 0

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name = ''

        contains

        procedure :: print => printChild
    end type

    class (base), pointer :: b1_m
    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fpAssgn019
use m
    interface
        subroutine print (b)
        use m
            class (base), intent(in), target :: b
        end subroutine
    end interface

    type (child), target :: c1
    class (child), allocatable :: c2
    type (base), target :: b1(10)

    c1 = child (10, 'c1')
    b1 = (/(base (i), i=1, 10)/)

    allocate (c2)

    c2%id = 20
    c2%name = 'c2'

    allocate (b1_m, source=child(100, 'b1_m'))

    call print (c1)
    call print (c2)

    call print (b1_m)
    call print (b1(4))
end


subroutine print (b)
use m
    class (base), intent(in), target :: b

    class (base), pointer :: b1
    type (base), pointer :: b2

    b1 => b

    b2 => b1

    call b1%print
    call b2%print
end subroutine
