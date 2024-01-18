! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/zeroSize/zeroSizeArray010.f
! opt variations: -ql

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/30/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Try a simple algorithm that finds the common
!                               subset of arrays of integer; then use it as the
!                               source for derived type component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind            :: k1
        integer(k1), allocatable :: data(:)

        contains

        procedure, private :: commonBase
        generic :: operator(.join.) => commonBase
    end type

    contains

    elemental type(base(4)) function commonBase (b1, b2)
        class(base(4)), intent(in) :: b1, b2

        if ((.not. allocated(b1%data)) .or. (.not. allocated(b2%data))) return

        commonBase%data = commonInteger(b1%data, b2%data)
    end function

    pure function commonInteger (i1, i2)
        integer, intent(in) :: i1(:), i2(:)

        integer bucket(min(size(i1), size(i2))), icount

        integer, allocatable :: commonInteger(:)

        icount = 0

        do i = 1, size(i1)
            do j = 1, size(i2)
                if (i1(i) == i2(j)) then
                    icount = icount + 1
                    bucket(icount) = i1(i)
                end if
            end do
        end do

        commonInteger = bucket(:icount)
    end function
end module

program zeroSizeArray010
use m
    type (base(4)), allocatable :: b1, b2, b3

    allocate (b1)

    b1%data = (/(i, i=0, 10000)/)

    b2 = b1 .join. base(4) ((/(i, i=-100000, -1)/))

    if (.not. allocated(b2)) error stop 1_4

    if (.not. allocated(b2%data)) error stop 2_4
    if (size(b2%data) /= 0) error stop 3_4

    b3 = b1 .join. base(4)((/(i, i=-10000, 10)/))

    if (.not. allocated(b3)) error stop 4_4
    if (.not. allocated(b3%data)) error stop 5_4

    if (any(b3%data /= (/0,1,2,3,4,5,6,7,8,9,10/))) error stop 6_4
end
