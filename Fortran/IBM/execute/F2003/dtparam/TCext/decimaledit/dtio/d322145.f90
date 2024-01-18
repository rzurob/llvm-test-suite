! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/decimaledit/dtio/d322145.f
! opt variations: -ql -qreuse=none

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/26/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               miscellaneous (defect 322145)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind     :: k1
        real(k1), pointer :: data(:) => null()
        real(k1)          :: r1 = -1.0

        contains

        procedure :: readBaseFmtd
        generic :: read (formatted) => readBaseFmtd
    end type

    contains

    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(4)), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (associated(dtv%data)) then
            do i = lbound(dtv%data,1), ubound(dtv%data, 1)
                read (unit, '(1x, e12.5)', iostat=iostat, iomsg=iomsg) &
                    dtv%data(i)
            end do
        else
            read (unit, '(1x)', iostat=iostat, iomsg=iomsg)
        end if

        if (iostat /= 0) return

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%r1
    end subroutine
end module

program dcmlChildWrite001
use m
    class(base(4)), allocatable :: b1(:)

    logical(4), external :: precision_r4

    allocate (b1(3))

    do i = 1, size(b1)
        allocate (b1(i)%data(i))
    end do

    read (*, '(DT"editDescriptor")', advance='no', size=isize) b1(2)

    if (.not. precision_r4(b1(2)%data(1), 1.23_4)) error stop 1_4

    if (.not. precision_r4(b1(2)%data(2), -2.33_4)) error stop 2_4

    if (.not. precision_r4(b1(2)%r1, 7.123_4)) error stop 3_4

    if (isize /= 32) error stop 4_4

    end
