! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/05/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Mix use of DT edit descriptor and DC/DP edit
!                               descriptors in a parent write statement.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class(*), allocatable :: data

        contains

        procedure :: writeBaseFmtd
        generic :: write(formatted) => writeBaseFmtd
    end type

    contains

    subroutine writeBaseFmtd(dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. allocated(dtv%data)) then
            iostat = 1000
            return
        end if

        select type (x => dtv%data)
            type is (real)
                write(unit, '(e15.6)', iostat=iostat, iomsg=iomsg) x

            type is (complex)
                write(unit, *, iostat=iostat, iomsg=iomsg) x

            class default
                write(unit, '(a)', iostat=iostat, iomsg=iomsg) 'unknown type'
        end select
    end subroutine
end module

program dcmlChildWrite006
use m
    open (1, file='dcmlChildWrite006.out', decimal='Comma')

    write (1, '(DT, DP, e15.6, dc)', advance='no') (base(i*1.2), i*1.2, i=1, 10)

    write (1, '(dp, a, dt)') new_line('a')

    write (1, '(Dp, dt, dc, 2e15.6, 1x, dt)') &
            (base(cmplx(i, i*2)), cmplx(i, i*2), base(i*1.0), i=1, 10)

end
