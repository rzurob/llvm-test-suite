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
!*  DATE                       : 07/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 322357)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
        complex(8), allocatable :: data(:)
        character(20) :: name
    end type

    private writeFormattedBase

    interface write(formatted)
        module procedure writeFormattedBase
    end interface

    contains

    subroutine writeFormattedBase (dtv, unit, iotype, v_list, iostat, iomsg)
        class (base), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype == 'LISTDIRECTED' .or. iotype == 'NAMELIST') then
            write (unit, '(f12.1)', pos=20, iostat=iostat, iomsg=iomsg) 1.2
        else
            write (unit, '(f12.1)', rec=20, iostat=iostat, iomsg=iomsg) 1.2
        end if
    end subroutine
end module

program dcmlChildWrite001d
use m
    character(200) :: c

    integer :: i1, i2

    write (c(1:100), '(DT)', decimal='comma', iostat=i1) &
            base(1, (/1.0, 2.0/), 'abcd')

    write (c(101:), *, decimal='Comma', iostat=i2) &
            base(2, (/3.0, 4.0/), 'efgh')

    if ((i1 /= i2) .or. (i1 == 0)) error stop 1_4
end
