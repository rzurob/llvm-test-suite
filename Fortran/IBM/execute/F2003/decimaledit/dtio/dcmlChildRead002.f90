!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/12/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the decimal= in parent READ takes
!                               effect during the child READ; test both internal
!                               and external files.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(4), allocatable :: data(:)

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

        integer isize

        read (unit, *, iostat=iostat, iomsg = iomsg) isize

        if (iostat /= 0) return

        if (allocated(dtv%data)) then
            deallocate (dtv%data)
        end if

        allocate(dtv%data(isize))

        read (unit, *, iostat=iostat, iomsg = iomsg) dtv%data
    end subroutine
end module

program dcmlChildRead002
use m
    real(4), allocatable :: r1(:)

    class (base), allocatable :: b1(:)

    logical(4), external :: precision_r4

    character(800) :: c

    allocate (r1(10), source=(/(i*1.1, i=1,10)/))

    allocate (b1(20))

    do i = 1, 20, 4
        allocate (b1(i)%data(50-i))
    end do

    write (c, *, decimal='comma') (i, r1(1:i), i=1, 10)

    write (1, *, decimal='comma') (i, (r1(j), j=1,i), i=1, 10)

    rewind 1

    read (1, *, decimal='comma') b1(::2)

    read (c(2:), '(dc, 10DT)') b1(2::2)

    do i = 1, 10
        do j = 1, i
            if (.not. precision_r4(b1(2*i-1)%data(j), j*1.1)) error stop 1_4

            if (.not. precision_r4(b1(2*i)%data(j), j*1.1)) error stop 2_4
        end do
    end do
end
