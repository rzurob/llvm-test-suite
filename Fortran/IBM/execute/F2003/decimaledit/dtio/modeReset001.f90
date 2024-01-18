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
!*  DATE                       : 07/12/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that if multiple child write statements
!                               ocurr during one parent write, each statement
!                               will NOT affect the subsequent child write
!                               statements.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        complex(8), allocatable :: cx

        contains

        procedure :: writeFormattedA

        generic :: write(formatted) => writeFormattedA
    end type

    contains

    subroutine writeFormattedA (dtv, unit, iotype, v_list, iostat, iomsg)
        class(A), intent(in) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)

        if (allocated(dtv%cx))then
            write(unit, *, iostat=iostat, iomsg=iomsg) dtv%cx

            if (iostat /= 0) return

            write(unit, *, iostat=iostat, iomsg=iomsg, decimal='COMMA') dtv%cx

            if (iostat /= 0) return

            write(unit, *, iostat=iostat, iomsg=iomsg) dtv%cx

        end if
    end subroutine
end module

program modeReset001
use m
    write(*, *, decimal='comma') A(1.0)
    write(*, *) A(1.0)

    write (*, '(DC,DT,/,DP,DT)') A(1.0d0), A(1.0d0)
end
