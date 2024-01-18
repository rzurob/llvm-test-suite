! GB DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/decimaledit/dtio/dcmlChildWrite001a.f
! opt variations: -qnock

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/05/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the decimal edit mode is set correctly
!                               during DTIO on internal file.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,k2,k3,n1)    ! (4,8,1,20)
        integer, kind             :: k1,k2,k3
        integer, len              :: n1
        integer(k1)                  id
        complex(k2), allocatable  :: data(:)
        character(kind=k3,len=n1) :: name
    end type

    private writeFormattedBase

    interface write(formatted)
        module procedure writeFormattedBase
    end interface

    contains

    subroutine writeFormattedBase (dtv, unit, iotype, v_list, iostat, iomsg)
        class (base(4,8,1,*)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. allocated (dtv%data)) then
            iostat = 1000
            iomsg = 'dtv%data must be allocated'
            return
        end if

        if (iotype == 'LISTDIRECTED' .or. iotype == 'NAMELIST') then
            write (unit, *, delim='quote', iostat=iostat, iomsg=iomsg) &
                    dtv%id, dtv%data, dtv%name
        else
            write (unit, fmt=iotype(3:), iostat=iostat, iomsg=iomsg) &
                    dtv%id, dtv%data, dtv%name
        end if
    end subroutine
end module

program dcmlChildWrite001a
use m
    character(200) :: c1, c2, c3

    character(200) :: answers(3)

    integer i1, i2, i3

    write (c1, *, decimal='Comma', iostat=i1) base(4,8,1,20)(1, (/1.0, 2.0/), 'abcd')

    write (c2, '(DT"(dc, i5,1x, 2(2en25.15, 1x), a)")', iostat=i2) &
            base(4,8,1,20)(2, (/.21, 3.1/), 'xlftest')

    write (c3, '(dc,DT"(i5,1x, 2(2en25.15, 1x), a)")', iostat=i3) &
            base(4,8,1,20)(3, (/.42, 5.2/), 'xlftest 101')

    if ((i1 /= 0) .or. (i2 /= 0) .or. (i3 /= 0)) error stop 1_4

    write (answers(1), *, decimal='comma', delim='Quote') 1, &
        cmplx(1.0, kind=8), cmplx(2.0, kind=8), 'abcd'//repeat(' ', 16)

    write (answers(2), '(dc, i5,1x, 2(2en25.15, 1x), a)') 2, &
        cmplx(.21, kind=8), cmplx(3.1, kind=8), 'xlftest'//repeat(' ', &
            20-len('xlftest'))

    write (answers(3), '(dc, i5,1x, 2(2en25.15, 1x), a)') 3, &
        cmplx(.42, kind=8), cmplx(5.2, kind=8), 'xlftest 101'//repeat(' ',9)


    if (c1 /= answers(1)) error stop 2_4
    if (c2 /= answers(2)) error stop 3_4

    if (c3 /= answers(3)) error stop 4_4
end
