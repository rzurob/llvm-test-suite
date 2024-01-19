! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly data used as
!                                selector; pointer assignment occurrs in
!                                ASSOCIATE block; also use array section)
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
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name

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

program fpAssgn030a2
use m
    class (base), allocatable, target :: b1 (:)

    allocate (b1 (2:11), source=child(0, 'b1'))

    associate (x => b1(::2))
        do i = 1, size (x)
            b1_m => x(i)

            b1_m%id = i*2

            call b1_m%print
        end do
    end associate

    do i = 2, 11
        call b1(i)%print
    end do
end
