! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (elemental PASS function
!*                               binding; basic comparason)
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

        procedure :: equal => baseEqual
    end type

    contains

    elemental logical*4 function baseEqual (b, b1)
        class (base), intent(in) :: b, b1

        baseEqual = (b%id == b1%id)
    end function

end module

program ftpbnd505a
use m
    type (base) :: b1, b2, b3(10), b4(10)
    logical*4 :: res(10)

    b1 = base(10)
    b2 = b1

    b3 = (/(base(id = i), i=3,12)/)

    b4 = b3

    !! elemental binding invoked by scalar
    if (.not. b1%equal(b2)) error stop 1_4

    if (.not. b1%equal (b1)) error stop 2_4


    !! invoked for array elements
    do i = 1, 10
        if (.not. b3(i)%equal(b4(i))) error stop 3_4
    end do


    !! invoked by array on array
    res = b4%equal(b3)

    if (.not. all (res)) error stop 4_4

    if (.not. all (b3%equal(b4))) error stop 5_4


    !! invoked by array on scalar
    res = b3%equal (b4(2))      ! res(2) is true, false for others

    if ((.not. res(2)) .or. res(1) .or. any(res(3:))) error stop 6_4

    !! invoked by scalar on array
    res = b3(2)%equal (b4)      ! res(2) is true, false for others

    if ((.not. res(2)) .or. res(1) .or. any(res(3:))) error stop 7_4
end