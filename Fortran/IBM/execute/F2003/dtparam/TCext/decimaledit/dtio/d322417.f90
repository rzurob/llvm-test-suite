! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/decimaledit/dtio/d322417.f
! opt variations: -qnok -qnol

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/05/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 322417)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: data

        contains

        procedure :: writeBaseFmtd
        generic :: write(formatted) => writeBaseFmtd
    end type

    contains

    subroutine writeBaseFmtd(dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(4,*)), intent(in) :: dtv
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
        end select
    end subroutine
end module

use m
    write (*, '(DT)', decimal='comma') (base(4,20)(i*1.0), i=1, 10)
end
