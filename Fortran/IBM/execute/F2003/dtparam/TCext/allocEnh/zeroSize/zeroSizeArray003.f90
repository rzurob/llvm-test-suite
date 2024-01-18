! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/allocEnh/zeroSize/zeroSizeArray003.f
! opt variations: -qnok -ql

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/29/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the zero-sized array of the derived type in
!                               the intrinsic assignment; use function results.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        class(*), allocatable :: data(:,:,:)
    end type
end module

program zeroSizeArray003
use m
    type(base(4)), allocatable :: b1(:), b2(:)

    !! test 1: directly assign zero-sized array
    b2 = (/base(4) :: /)

    b1 = b2

    if (.not.(allocated(b1)) .or. (.not. allocated(b2))) error stop 1_4

    if ((size(b1) /= 0) .or. (size(b2) /= 0)) error stop 2_4

    !! test 2: assign zero-sized array from a function call
    b1 = genBaseArray(10)

    b2 = genBaseArray(-10)

    if ((size(b1) /= 10) .or. (size(b2) /= 0)) error stop 3_4

    do i = 1, 10
        if (.not. allocated(b1(i)%data)) error stop 4_4

        select type (x => b1(i)%data)
            type is (base(4))
                if (any(shape(x) /= (/1,2,3/))) error stop 5_4

                do j = 1, 2
                    do k = 1, 3
                        if (allocated(x(1,j,k)%data)) error stop 6_4
                    end do
                end do

            class default
                error stop 8_4
        end select
    end do

    contains

    type(base(4)) function genBaseArray (i)
        integer, intent(in) :: i

        dimension genBaseArray(i)

        genBaseArray = base(4)(reshape((/(base(4)(null()), i=1,6)/), (/1,2,3/)))
    end function
end
