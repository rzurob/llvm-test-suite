! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/decimaledit/dtio/dcmlChildWrite004.f
! opt variations: -ql -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/05/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the decimal edit mode is also set
!                               during DTIO if the file is opened for.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,k2,k3)    ! (4,8,4)
        integer, kind     :: k1,k2,k3
        integer(k1)          id
        real(k2), pointer :: data
        logical(k3)          flag

        contains

        procedure :: writeBaseFmtd
        generic :: write(formatted) => writeBaseFmtd
    end type

    type, extends(base) :: child    ! (4,8,4)
        class(*), allocatable :: x

        contains

        procedure :: writeBaseFmtd => writeChildFmtd
    end type

    contains

    subroutine writeBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(4,8,4)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. associated(dtv%data)) then
            iostat = 1000
            return
        end if

        write (unit, '(i5,1x, d20.12, 1x, l5)', iostat=iostat, iomsg=iomsg) &
            dtv%id, dtv%data, dtv%flag
    end subroutine

    subroutine writeChildFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(child(4,8,4)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        write (unit, '(DT)', decimal='Point', iostat=iostat, iomsg=iomsg) &
            dtv%base

        if (iostat /= 0) return

        select type (x => dtv%x)
            type is (real(8))
                write (unit, '(e20.12)') x

            class default
                iostat = 2000
                return
        end select
    end subroutine
end module

program dcmlChildWrite004
use m
    class (base(4,8,4)), allocatable :: b1, b2(:)

    double precision, target :: d1(10)

    open (1, file='dcmlChildWrite004.out', decimal='comma', access='stream', &
        form='formatted')

    allocate (b1, source=base(4,8,4)(1, null(), .true.))

    allocate (b1%data, source= -1.2d0)

    allocate (b2(size(d1)), source=(/(child(4,8,4)(i*10, d1(i), mod(i,3) == 0, i*1.0d1),&
            i = 1, 10)/))


    d1 = (/(i*11, i=1,10)/)

    write (1, '(DT)') b1, b2

    close(1)
end
