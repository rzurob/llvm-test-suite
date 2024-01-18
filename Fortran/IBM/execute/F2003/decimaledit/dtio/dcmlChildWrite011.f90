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
!                               Test that the decimal modes are correctly set
!                               for different components for the same derived
!                               type.
!                               Also tests that "DP", "DC" in the quoted string
!                               literal in the format specification has no
!                               effect on decimal edit mode.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        real, allocatable :: data

        contains

        procedure :: writeAFmtd
        generic :: write(formatted) => writeAFmtd
    end type

    type base
        type(A) a1
        type(A) a2
    end type

    contains

    subroutine writeAFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(A), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        type AA
            real data
        end type

        type(AA) a2

        namelist /base/ a2

        if (iotype(1:2) == 'DT') then
            if (iotype == 'DT') then
                write (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
            else
                a2%data = dtv%data

                write (unit, base, iostat=iostat, iomsg=iomsg)
            end if
        end if
    end subroutine
end module

program dcmlChildWrite011
use m
    write (*,*) 'test 1'
    write (*, '("DP ", DT,/,DT"abc")', decimal='Comma') base(A(1.0), A(2.0))
    write (*,*) 'test 2'
    write (*, '("DP",e12.4,/,dp, DT"dc")', decimal='Comma') 1.0, A(2.0)
    write (*,*) 'test 3'
    write (*, "('dc', DT'DP', dc,/, 'DP', DT, 'abc')") base(A(1.0), A(2.0))

end
