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
!*  DATE                       : 09/12/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Use the intrinsic assignment in a DTIO routine
!                               (write statement).
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real, allocatable :: r1(:)
        real, allocatable :: r2(:,:)

        contains

        procedure :: writeBaseFmtd
        generic :: write(formatted) => writeBaseFmtd
    end type

    contains

    subroutine writeBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        real, allocatable :: localVar(:)

        if (allocated(dtv%r1)) then
            if (allocated(dtv%r2)) then
                localVar = (/dtv%r1, dtv%r2/)
            else
                localVar = (/dtv%r1/)
            end if
        else
            if (allocated(dtv%r2)) then
                localVar = (/dtv%r2/)
            end if
        end if

        if (allocated(localVar)) then
            write (unit, '(5g12.4)', iostat=iostat, iomsg=iomsg) localVar
        else
            write (unit, *, iostat=iostat, iomsg=iomsg) 'no data to write'
        end if

        if (iostat /= 0) return

        write (unit, *) new_line('a')
    end subroutine
end module

program dtio002
use m
    type(base), allocatable :: b1(:), b2(:)

    b1 = (/(base(null(), null()), i=1,10)/)

    print *, b1

    b2 = (/(base((/(j, j=1,i)/),reshape((/(j, j=1,i*i)/), (/i,i/))), &
        base((/(j, j=1,i)/), null()), base(null(), reshape((/(j, j=1,i*i)/), &
        (/i,i/))), i=1,3)/)

    write (*, '(9DT)') b2
end
