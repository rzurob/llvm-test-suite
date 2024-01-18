!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio519.f
! %VERIFY: fdtio519.out:fdtio519.vf
! %STDIN:
! %STDOUT: fdtio519.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 01/14/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : DTIO generics (DTIO for derived types with
!                               private component)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), private :: data(2)

        contains

        procedure :: print => printBase
        procedure :: setVal => updateBase
        procedure :: getVal => getBaseVal
    end type


    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface


    contains

    subroutine updateBase (b, val)
        class (base), intent(inout) :: b
        real(8), intent(in) :: val(2)

        b%data = val
    end subroutine

    real(8) function getBaseVal (b)
        class (base), intent(in) :: b
        dimension getBaseVal(2)

        getBaseVal = b%data
    end function


    subroutine printBase (b, unit, iostat, iomsg)
        class (base), intent(in) :: b
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        write (unit, '(10x, 2g10.2,/)', iostat=iostat, iomsg=iomsg) b%getVal()
    end subroutine
end module


subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size (v_list) /= 0) error stop 10_4

    call dtv%print(unit, iostat, iomsg)
end subroutine
program fdtio519
use m
    class (base), allocatable :: b1(:)

    integer stat1
    character(200) err

    allocate (b1(2))


    call b1(1)%setVal ((/1.3_8, 2.1_8/))
    call b1(2)%setVal ((/-2.4_8, -3.2_8/))

    !! test scalars
    print *, b1(1)

    !! test arrays
    print *, b1
end
