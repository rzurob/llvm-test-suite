! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/decimaledit/dtio/modeReset001.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/12/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that if multiple child write statements
!                               ocurr during one parent write, each statement
!                               will NOT affect the subsequent child write
!                               statements.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A(n1,k1)    ! (20,8)
        integer, kind            :: k1
        integer, len             :: n1
        complex(k1), allocatable :: cx

        contains

        procedure :: writeFormattedA

        generic :: write(formatted) => writeFormattedA
    end type

    contains

    subroutine writeFormattedA (dtv, unit, iotype, v_list, iostat, iomsg)
        class(A(*,8)), intent(in) :: dtv
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
    write(*, *, decimal='comma') A(20,8)(1.0)
    write(*, *) A(20,8)(1.0)

    write (*, '(DC,DT,/,DP,DT)') A(20,8)(1.0d0), A(20,8)(1.0d0)
end
