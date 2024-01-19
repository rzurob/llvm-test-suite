! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/decimaledit/dtio/dcmlChildWrite013.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/09/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test the use of array sections in the child
!                               write statement for a namlist case in comma edit
!                               mode.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind         :: k1
        real(k1), allocatable :: data(:)

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

        if (iotype /= 'NAMELIST') error stop 10_4

        if (allocated(dtv%data)) then
            write (unit, '(5e15.4)', iostat=iostat, iomsg=iomsg) &
                dtv%data(::2)

            if (iostat /= 0) return

            if (size(dtv%data) > 1) then
                write (unit, '(/, dp, 5(e15.4))', iostat=iostat, iomsg=iomsg) &
                    dtv%data(lbound(dtv%data,1)+1::2)
            end if

        end if
    end subroutine
end module

program dcmlChildWrite013
use m
    class (base(4)), allocatable :: b1
    class(base(4)), pointer :: b2(:)

    namelist /nml1/ b1, b2

    real :: r1(0:15) = (/(i*1.0, i=0,15)/)

    open (9, file="dcmlChildWrite013.out", decimal='comma')

    allocate (b1, source=base(4)(r1))

    allocate (b2(5), source=(/(base(4)((/(1.2**j, j=1, i)/)), i=1,5)/))

    write(9, nml1)
end
