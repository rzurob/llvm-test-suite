!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/25/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the defined operation for derived type with
!                               character allocatable component; function return
!                               an allocatable of deferred character string.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        character(:), allocatable :: str

        contains

        procedure :: concat => concatBaseStr
        generic :: operator(//) => concat
    end type

    contains

    character(:) function concatBaseStr (b, c)
        class(base), intent(in) :: b
        character(*), intent(in) :: c

        allocatable concatBaseStr

        if (allocated(b%str)) then
            concatBaseStr = b%str // c
        else
            concatBaseStr = c
        end if
    end function
end module

program definedOp007
use m
    type(base), allocatable :: b1(:)

    b1 = [(base('xlftest'), i=1, 2), base(null())]


    b1(2) = base(b1(1)//' 101 ' // b1(2)%str)

    b1(1) = base(b1(3) // ' 101 ' // b1(1)%str)

    if ((.not. allocated(b1(1)%str)) .or. &
        (.not. allocated(b1(2)%str))) error stop 1_4

    if (b1(1)%str /= ' 101 xlftest') error stop 2_4

    if (b1(2)%str /= 'xlftest 101 xlftest') error stop 3_4

    if (allocated(b1(3)%str)) error stop 4_4
end
