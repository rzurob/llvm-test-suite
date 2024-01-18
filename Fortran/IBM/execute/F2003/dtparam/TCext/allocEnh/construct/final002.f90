! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/construct/final002.f
! opt variations: -ql

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/8/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the finalization process during the
!                               intrinsic assignment for scalars.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind     :: k1
        real(k1), pointer :: data(:)

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b1)
        type(base(4)), intent(inout) :: b1

        print *, 'finalizeBase'

        if (associated(b1%data)) then
            print *, 'deallocating b1%data; shape:', shape(b1%data)

            deallocate(b1%data)
        end if
    end subroutine
end module

program final002
use m
    type(base(4)), allocatable :: b1, b11
    type(base(4)), pointer :: b2
    type(base(4)) b3, b4

    logical(4), external :: precision_r4

    allocate (b1, b2)

    allocate(b1%data(100), b2%data(3))

    allocate (b3%data(10), b4%data(0))

    b3%data = log([(i*1.0e1, i=1,10)])

    print *, '1'
    !! finalization will happen; deallocate b1%data's 100 elements
    b1 = b3

    print *, '2'
    !! finalization will happen; deallocate b2%data's 3 elements
    b2 = b1

    print *, '3'
    !! no finalization to happen; allocate value for b11
    b11 = b4

    print *, '4'
    !! finalization will happen; deallocate b11%data; 0 sized array
    b11 = b2

    if (.not. allocated(b11)) error stop 1_4

    if ((size(b1%data) /= 10) .or. (size(b2%data) /= 10) .or. &
        (size(b11%data) /= 10)) error stop 2_4

    if ((.not. associated(b1%data, b3%data)) .or. &
        (.not. associated(b2%data, b3%data)) .or. &
        (.not. associated(b11%data, b3%data))) error stop 3_4


    do i = 1, 10
        if (.not. precision_r4 (b1%data(i), log(i*10.))) error stop 4_4
    end do
end

