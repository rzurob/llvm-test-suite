! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol /tstdev/F2003/allocEnh/funcResult/definedOp007.f
! opt variations: -qnock -qnok -ql

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
    type base(k1)    ! (4)
        integer, kind :: k1
        character(:), allocatable :: str

        contains

        procedure :: concat => concatBaseStr
        generic :: operator(//) => concat
    end type

    contains

    character(:) function concatBaseStr (b, c)
        class(base(4)), intent(in) :: b
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
    type(base(4)), allocatable :: b1(:)

    b1 = [(base(4)('xlftest'), i=1, 2), base(4)(null())]


    b1(2) = base(4)(b1(1)//' 101 ' // b1(2)%str)

    b1(1) = base(4)(b1(3) // ' 101 ' // b1(1)%str)

    if ((.not. allocated(b1(1)%str)) .or. &
        (.not. allocated(b1(2)%str))) error stop 1_4

    if (b1(1)%str /= ' 101 xlftest') error stop 2_4

    if (b1(2)%str /= 'xlftest 101 xlftest') error stop 3_4

    if (allocated(b1(3)%str)) error stop 4_4
end
