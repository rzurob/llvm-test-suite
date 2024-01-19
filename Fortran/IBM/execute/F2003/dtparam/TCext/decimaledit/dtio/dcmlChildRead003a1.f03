! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/decimaledit/dtio/dcmlChildRead003a1.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/13/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Smiliar to dcmlChildRead003 but tests the fact
!                               that end of record is treated as if blanks in
!                               list-directed read.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,k2)    ! (4,8)
        integer, kind            :: k1,k2
        real(k1), allocatable    :: data(:)
        complex(k2), allocatable :: cx(:)

        contains

        procedure :: readBaseFmtd
        generic :: read(formatted) => readBaseFmtd
    end type

    contains

    !! read in the array size before allocating dtv's components
    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(4,8)), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        integer isize

        read (unit, *, iostat = iostat, iomsg=iomsg) isize

        if (iostat /= 0) return

        if (allocated(dtv%data)) deallocate (dtv%data)

        allocate (dtv%data(isize))

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data

        if (iostat /= 0) return

        read (unit, *, iostat = iostat, iomsg=iomsg) isize

        if (iostat /= 0) return

        if (allocated(dtv%cx)) deallocate (dtv%cx)

        allocate (dtv%cx(isize))

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%cx
    end subroutine
end module

program dcmlChildRead003a1
use m
    integer currentPos

    type(base(4,8)), allocatable :: b1(:)

    logical(4), external :: precision_x6, precision_r4

    open (1, file='dcmlChildRead003.data', access='stream', form='formatted', &
            decimal='Comma')

    write (1, '(i4, 10(g15.7))', pos=1, decimal='Point') 10, (i*1.0, i=1, 10)

    inquire (1, pos=currentPos)

    write(1, *, pos=currentPos, decimal='point') 12, (cmplx(i*1.0, i*2.0, 8), i=-12, -1)

    write (1, *) 20, (i*1.22, i=1,20)
    write (1, *, sign='plus') 22, (cmplx(i*1.1, i*2.2,8), i=-10,11)

    !! now read data back to b1

    allocate (b1(2))

    allocate (b1(1)%cx(3), b1(2)%data(1000))

    read (1, '(dp, dt, dc, dt)', pos=1) b1

    if ((.not. allocated(b1(1)%data)) .or. (.not. allocated(b1(1)%cx))) &
            error stop 1_4

    if ((.not. allocated(b1(2)%data)) .or. (.not. allocated(b1(2)%cx))) &
            error stop 2_4

    if ((size(b1(1)%data) /= 10) .or. (size(b1(1)%cx) /= 12)) error stop 3_4

    if ((size(b1(2)%data) /= 20) .or. (size(b1(2)%cx) /= 22)) error stop 4_4

    do i = 1, 10
        if (.not. precision_r4 (b1(1)%data(i), i*1.0)) error stop 5_4
    end do

    do i = 1, 12
        if (.not. precision_x6 (b1(1)%cx(i), cmplx((i-13)*1.0, (i-13)*2.0,8)))&
                error stop 6_4
    end do

    do i = 1, 20
        if (.not. precision_r4(b1(2)%data(i), i*1.22)) error stop 7_4
    end do

    do i = 1, 22
        if (.not. precision_x6(b1(2)%cx(i), cmplx((i-11)*1.1, (i-11)*2.2, 8)))&
                error stop 8_4
    end do
end
