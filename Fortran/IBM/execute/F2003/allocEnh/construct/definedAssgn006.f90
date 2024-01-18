!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/18/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Use a structure that contains two components,
!                               both having type-bound assignment defined.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type number
        integer, allocatable :: id

        contains

        procedure :: assgnID
        generic :: assignment(=) => assgnID
    end type

    type string
        character(:), allocatable :: str

        contains

        procedure :: assgnStr
        generic :: assignment(=) => assgnStr
    end type

    contains

    elemental subroutine assgnID (a1, a2)
        class(number), intent(out) :: a1
        class(number), intent(in) :: a2

        if (allocated(a2%id)) a1%id = a2%id + 10000
    end subroutine

    elemental subroutine assgnStr (b1, b2)
        class(string), intent(out) :: b1
        class(string), intent(in) :: b2

        if (allocated(b2%str)) b1%str = ' ' // b2%str
    end subroutine
end module

module n

use m
    type employee
        type(number) :: ID
        type(string) :: name(2)
    end type
end module

use n
    type(employee), allocatable :: p1(:), p2
    type(number) num1
    type(string) name(2)

    num1%id = 1

    name(1)%str = 'Mr'
    name(2)%str = 'XYZ'

    p2 = employee(num1, name)

    if ((.not. allocated(p2%id%id)) .or. &
        (.not. allocated(p2%name(1)%str)) .or. &
        (.not. allocated(p2%name(2)%str))) error stop 1_4


    if (p2%id%id /= 10001) error stop 2_4

    if ((len(p2%name(1)%str) /= 3) .or. &
        (len(p2%name(2)%str) /= 4)) error stop 3_4

    if ((p2%name(1)%str /= ' Mr') .or. &
        (p2%name(2)%str /= ' XYZ')) error stop 4_4
end
