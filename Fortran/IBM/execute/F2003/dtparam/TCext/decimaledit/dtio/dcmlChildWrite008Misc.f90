! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv /tstdev/F2003/decimaledit/dtio/dcmlChildWrite008Misc.f
! opt variations: -ql -qdefaultpv -qreuse=self

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 07/10/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               miscellaneous (program seg faults under qsmp)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind :: k1
        complex(k1)      cx(2)

        contains

        procedure :: writeBaseFmtd
        generic :: write(formatted) => writeBaseFmtd
    end type

    contains

    !! subroutine controls precision in writing complex; but in a format
    !similar to listed-directed write
    subroutine writeBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(8)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(5), allocatable :: mode

        character, parameter :: separator(2) = (/',', ';'/)

        character currSeparator

        allocate (mode)

        inquire (unit, decimal=mode)

        if (mode == 'COMMA') then
            currSeparator = separator(2)
        else if (mode == 'POINT') then
            currSeparator = separator(1)
        else
            error stop 10_4
        end if

        write (unit, '(2(" (", d18.10, a, d18.10, " )"))', &
            iostat=iostat, iomsg=iomsg) (real(dtv%cx(i)), currSeparator, &
                aimag(dtv%cx(i)), i=1,2,1)
    end subroutine
end module

program dcmlChildWrite008
use m
    type A(k2)    ! (8)
        integer, kind  :: k2
        real(k2)       :: val
        type(base(k2)) :: data
    end type

    write (1, '(dc, d18.10, 1x, DT"no effect")') A(8)(1.25d0, base(8)((/2.3d0, 1.2d0/)))
end
