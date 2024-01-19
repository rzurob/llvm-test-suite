! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/21/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the slashes (/) encountered in the
!                               input file terminates the particular child READ
!                               statement that encounters slash.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
        real, allocatable :: data(:)
        complex cx

        contains

        procedure :: readBaseFmtd
        generic :: read(formatted) => readBaseFmtd
    end type

    contains

    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. allocated(dtv%data)) then
            iostat = 1000
            iomsg = 'allocate data first before read'
            return
        end if

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%id

        if (iostat /= 0) return

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data

        if (iostat /= 0) return

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%cx
    end subroutine
end module

program dcmlChildRead012
use m
    class(base), allocatable :: b1(:)

    integer unit

    logical(4), external :: precision_r4, precision_x8

    unit = 1

    call prepareData (unit)

    allocate (b1(6))

    do i = 1, 6
        allocate (b1(i)%data(i+2), source=-1.0)
    end do

    read (unit, *) b1

    !! verify b1
    do i = 1, 6
        if (b1(i)%id /= i) error stop 1_4

        if (.not. precision_r4(b1(i)%data(1), i*1.11_4)) error stop 2_4

        if (.not. precision_r4(b1(i)%data(2), (i+1)*1.11_4)) error stop 3_4

        do j = 3, i+2
            if (.not. precision_r4(-1.0_4, b1(i)%data(j))) error stop 4_4
        end do

        if (.not. precision_x8(b1(i)%cx, cmplx(2*i, i**2, 4))) error stop 5_4
    end do
end

subroutine prepareData (unit)
integer, intent(in) :: unit
    open (unit, file='dcmlChildRead012.data', decimal='coMMa')

    write (unit, 100) (i, (j*1.11, j=i,i+1), cmplx(2*i, i**2), i=1,6)

    rewind (unit)

100 format (i5, 1x, e15.8, 1x, e15.8, " /", /, " (", e15.8, " ; ", e15.8, ') ')
end
