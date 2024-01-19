! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (inherited binding called
!*                               in the new binding in the extended type)
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
        integer*4, private :: id = 0

        contains

        procedure, non_overridable :: assgnID => assgnBaseID
        procedure, non_overridable :: getID => getBaseID
    end type

    contains

    subroutine assgnBaseID (b, i)
        class (base), intent(out) :: b
        integer*4, intent(in) :: i

        b%id = i
    end subroutine

    integer*4 function getBaseID (b)
        class (base), intent(in) :: b

        getBaseID = b%id
    end function
end module

module m1
use m, only : base

    type, extends (base) :: child
        character*20 :: name = ''

        contains

        procedure :: equal => equalChild
        procedure :: copy => copyChild
    end type

    contains

    logical*4 function equalChild (b, b1)
        class (child), intent(in) :: b
        type (child), intent(in) :: b1

        equalChild = ((b%getID() == b1%getID()) .and. (b%name == b1%name))
    end function

    subroutine copyChild (b, b1)
        class (child), intent(out) :: b
        type (child), intent(in) :: b1

        call b%assgnID (b1%getID())
        b%name = b1%name
    end subroutine
end module

program ftpbnd517
use m1
    type (child) :: c1, c2
    class (child), allocatable :: c3(:)

    call c1%assgnID (10)

    if (c1%equal(c2) .or. c2%equal(c1)) error stop 1_4

    call c2%copy(c1)

    if (.not. c2%equal(c1)) error stop 2_4

    if (c2%getID() /= 10) error stop 3_4

    call c1%base%assgnID (c1%base%getID()+10)

    if (c1%getID() /= c1%base%getID()) error stop 4_4

    if (c1%getID() /= 20) error stop 5_4


    allocate (c3(2))

    call c3(1)%assgnID (100+c3(1)%getID())

    call c3(2)%base%assgnID (200)

    if ((c3(1)%getID() /= 100) .or. (c3(2)%base%getID() /= 200)) error stop 6_4

    if (c3(1)%equal(c3(2))) error stop 7_4

    call c3(1)%copy (c3(2))

    if (c3(1)%getID() /= 200) error stop 8_4

    if (.not. c3(2)%equal(c3(1))) error stop 9_4
end
