! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/28/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the decimal edit mode is set
!                               eventually by DC/DP edit descriptor in the
!                               parent format-specification (even a conflicting
!                               decimal edit mode is used in decimal= specifier
!                               in WRITE statement).
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real, pointer :: data(:) => null()
        complex(8) :: cx = (-1.0, -1.0)

        contains

        procedure :: writeBaseFmtd
        generic :: write (formatted) => writeBaseFmtd
    end type

    contains

    subroutine writeBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (associated(dtv%data)) then
            do i = lbound(dtv%data,1), ubound(dtv%data, 1)
                write (unit, '(1x, e12.5)', iostat=iostat, iomsg=iomsg) &
                    dtv%data(i)
            end do
        else
            write (unit, '(a)', iostat=iostat, iomsg=iomsg) &
                    'dtv%data is not associated'
        end if

        if (iostat /= 0) return

        write (unit, *, iostat=iostat, iomsg=iomsg) dtv%cx

        if (iostat /= 0) return

        write (unit, '(/)')
    end subroutine
end module

program dcmlChildWrite002
use m
    class(base), allocatable :: b1(:)

    allocate (b1(10))

    do i = 1, size(b1)
        allocate (b1(i)%data(i), source=(/(j*1.0, j=1, i)/))
    end do

    open (1, file='dcmlChildWrite002.data')

    write (1, '(dp, e12.4, (5DT))', decimal='cOmma', advance='no') 1.2, b1

    close(1)
end