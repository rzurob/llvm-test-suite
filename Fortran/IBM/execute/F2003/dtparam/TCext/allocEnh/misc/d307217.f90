! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/misc/d307217.f
! opt variations: -ql

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
!*  DATE                       : 10/17/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 307217)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind            :: k1
        integer(k1), allocatable :: id
    end type
    
    interface write (formatted)
        subroutine writeFormatted (dtv, unit, iotype, vlist, iostat, iomsg)
        import base
            class (base(4)), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    private write(formatted)

    contains

    subroutine printBase (b)
        class(base(4)), intent(in) :: b

        print *, b
    end subroutine

    subroutine printBaseArray (b, n)
        class(base(4)), intent(in) :: b(*)
        integer, intent(in) :: n

        print *, b(1:n)
    end subroutine
end module

use m
    type(base(4)), allocatable :: b1(:,:)

    call printBase(base(4)(null()))
    call printBase(base(4)(10))

    b1 = reshape([(base(4)(null()), i = 1, 20)], [4,5])

    do i = 1, 4, 2
        do j = 1, 5
            b1(i,j) = base(4)(i*10+j)
        end do
    end do

    call printBaseArray(b1, 20)
end

subroutine writeFormatted (dtv, unit, iotype, vlist, iostat, iomsg)
use m, only: base
    class (base(4)), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated(dtv%id)) then
        write (unit, *, iostat = iostat, iomsg=iomsg) dtv%id
    else
        write (unit, *, iostat = iostat, iomsg=iomsg) 'NULL'
    end if
end subroutine
