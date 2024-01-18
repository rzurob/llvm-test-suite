!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/27/2005
!*
!*  DESCRIPTION                : data pointer assignment (BIND(C) type pointer
!                               assigned to unlimted poly targets)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
use ISO_C_BINDING
    interface makeData
        module procedure makeBtypeData1
        module procedure makeBtypeData2
    end interface

    type, bind(c) :: bType
        integer(C_SHORT) :: i
        integer(c_long) :: j
    end type

    contains

    class(*) function makeBtypeData1 (i1)
        pointer makeBtypeData1
        integer, intent(in) :: i1(:)

        nullify (makeBtypeData1)

        if (size(i1) == 2) then
            allocate (makeBtypeData1, source=bType(i1(1), i1(2)))
        end if
    end function

    class(*) function makeBtypeData2 (i1, isize)
        pointer makeBtypeData2 (:)
        integer, intent(in) :: i1(:)
        integer, intent(in) :: isize

        nullify (makeBtypeData2)

        if (size(i1) == 2) then
            allocate (makeBtypeData2(isize), source=bType(i1(1), i1(2)))
        end if
    end function
end module

program fpAssgn009a6
use m
    type(bType), pointer :: b1, b2(:)

    !! first test
    b1 => makeData((/1, 10/))

    b2 => makeData((/-1, -10/), 2)

    if ((.not. associated(b1)) .or. (.not. associated(b2))) error stop 1_4

    if ((b1%i /= 1) .or. (b1%j /= 10)) error stop 2_4

    if (size(b2) /= 2) error stop 3_4

    if (any(b2%i /= -1) .or. any (b2%j /= -10)) error stop 4_4

    deallocate (b1, b2)

    !! 2nd test
    b1 => makeData ((/1/))
    b2 => makeData ((/-1/), 10)

    if (associated (b1) .or. associated(b2)) error stop 5_4
end
