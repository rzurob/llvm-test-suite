! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/decimaledit/dtio/dcmlChildWrite001.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/27/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the decimal edit mode is carried to
!                               child data transfer in write statement by
!                               decimal= in parent write statement.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1,k2)    ! (20,4,8)
        integer, kind     :: k1,k2
        integer, len      :: n1
        real(k1), pointer :: data(:) => null()
        complex(k2)       :: cx = (-1.0, -1.0)

        contains

        procedure :: writeBaseFmtd
        generic :: write (formatted) => writeBaseFmtd
    end type

    contains

    subroutine writeBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(*,4,8)), intent(in) :: dtv
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

program dcmlChildWrite001
use m
    class(base(:,4,8)), allocatable :: b1(:)

    allocate (base(20,4,8) :: b1(10))

    do i = 1, size(b1)
        allocate (b1(i)%data(i), source=(/(j*1.0, j=1, i)/))
    end do

    open (1, file='dcmlChildWrite001.data')

    write (1, *) b1(::2)

    write (1, *, decimal='cOmma') b1(2::2)
end
