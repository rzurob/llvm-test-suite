! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/decimaledit/dtio/dcmlChildWrite006a.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/10/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test of the implied-do loop involving the
!                               derived type with pointer component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind     :: k1
        real(k1), pointer :: data(:) => null()

        contains

        procedure :: writeBaseFmtd
        generic :: write(formatted) => writeBaseFmtd
    end type

    contains

    subroutine writeBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(4)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(5), parameter :: decMode(0:1) = (/'Point', 'Comma'/)

        if (.not. associated(dtv%data)) return

        do i = lbound(dtv%data, 1), ubound(dtv%data, 1)
            write(unit, '(e12.5, 1x)', decimal=decMode(mod(i,2)), &
                iostat=iostat, iomsg=iomsg) dtv%data(i)

            if (iostat /= 0) return
        end do

        write (unit, '(e12.5)') i*1.1
    end subroutine

    function genBase (r1)
        class(base(4)), allocatable :: genBase
        real, intent(in) :: r1(:)

        allocate(genBase)

        allocate(genBase%data(size(r1)), source=r1)
    end function
end module

program dcmlChildWrite006a
use m
    type A(k2)    ! (4)
        integer, kind     :: k2
        real(k2), pointer :: data(:)
    end type

    type (A(4)) a1(10)

    do i = 1, 10
        allocate (a1(i)%data(0:i), source=(/(j*1.1, j=0, i)/))
    end do

    write (1, '(dp,DT)', decimal='comma') (base(4)(a1(i)%data), i=1,10)

    write (1, *) 'test 2'

    write (1, '(dc, dt)') (genBase(a1(i)%data), i=1,10)
end
