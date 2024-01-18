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
! %GROUP: fdtio056.f
! %VERIFY: fdtio056.out:fdtio056.vf
! %STDIN:
! %STDOUT: fdtio056.out
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
!*  DATE                       : 11/30/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : DTIO generics (handle the inheritance
!                               relationship in DTIO)
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
        real(8) r1
    end type

    type, extends(base) :: child
        character(15) :: name
        integer(8) :: id
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

end module

program fdtio056
use m
    class (base), allocatable :: b1(:)

    type (child) c1(3)

    c1%r1 = (/(2.0*i, i=1,3)/)

    c1%name = (/'c1_1', 'c1_2', 'c1_3'/)
    c1%id = (/3, 2, 1/)

    allocate (b1(0:2), source=c1)

    print *, c1

    write(*,*) b1
end


subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only:base, child
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, '(a, g9.2)', iostat=iostat, iomsg=iomsg) 'r1=', dtv%r1

    if (iostat /= 0) return

    select type (dtv)
        type is (child)
            write (unit, *, iostat=iostat, iomsg=iomsg) 'name=', dtv%name, &
                    'id=', dtv%id, "|"
        type is (base)

        class default
            error stop 10_4
    end select
end subroutine
