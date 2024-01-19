! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/13/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that DC/DP in the parent READ statement
!                               takes effect during the child READ statement;
!                               test external files.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real, allocatable :: data(:)
        complex(8), allocatable :: cx(:)

        contains

        procedure :: readBaseFmtd
        generic :: read(formatted) => readBaseFmtd
    end type

    contains

    !! read in the array size before allocating dtv's components
    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(inout) :: dtv
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

program dcmlChildRead003
use m
    integer currentPos

    type(base), allocatable :: b1

    logical(4), external :: precision_x6, precision_r4

    open (1, file='dcmlChildRead003.data', access='stream', form='formatted')

    write (1, '(i4,dc, 10(" ;", g15.7))', pos=1, advance='no') &
            10, (i*1.0, i=1, 10)

    inquire (1, pos=currentPos)

    write(1, *, pos=currentPos, decimal='Comma') 12, (cmplx(i*1.0, i*2.0, 8), i=-12, -1)

    !! now read data back to b1

    allocate (b1)

    allocate (b1%cx(3))

    read (1, '(dc, dt)', pos=1) b1

    if ((.not. allocated (b1%data)) .or. (.not. allocated(b1%cx))) &
            error stop 1_4

    if ((size(b1%data) /= 10) .or. (size(b1%cx) /= 12)) error stop 2_4

    do i = 1, 10
        if (.not. precision_r4(b1%data(i), i*1.0)) error stop 3_4
    end do

    do i = 1, 12
        if (.not. precision_x6(b1%cx(i), cmplx((i-13)*1.0, (i-13)*2.0,8))) &
                error stop 4_4
    end do
end
