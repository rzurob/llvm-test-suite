
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

program dcmlChildRead003a
use m
    type(base), allocatable :: b1

    logical(4), external :: precision_r4, precision_x6

    character(:), allocatable :: c

    allocate (character(1000) :: c)

    write (c, '(dc, i4,10(";",g15.7), i4,12(";(",g25.16, ";", g25.16,")" ))') &
            10, (i*1.0, i=1, 10), 12, (cmplx(i*1.0, i*2.0, 8), i=-12,-1)


    !! now read data back to b1
    allocate (b1)

    allocate (b1%cx(3))

    read (c, '(dc, dt)') b1

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
