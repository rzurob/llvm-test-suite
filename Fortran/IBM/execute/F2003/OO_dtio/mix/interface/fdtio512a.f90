!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio512a.f
! %VERIFY: fdtio512a.out:fdtio512a.vf
! %STDIN:
! %STDOUT: fdtio512a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (list-directed write for function
!                               result that is a rank one poly-array pointer)
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
        integer(8), allocatable :: id
    end type

    type, extends(base) :: child
        character(20) :: name
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

    class (base) function produceBasePtr (id, name, size)
        pointer produceBasePtr(:)
        integer(8), intent(in) :: id
        character(*), optional, intent(in) :: name
        integer, intent(in) :: size

        if (present (name)) then
            allocate (produceBasePtr(size), source=child(id, name))
        else
            allocate (produceBasePtr(size), source=base(id))
        end if
    end function
end module

program fdtio512a
use m

    print *, produceBasePtr (100_8, size=2)
    print *, produceBasePtr (200_8, 'xlftest team', 3)
end


subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%id)) write (unit, *, iostat=iostat, iomsg=iomsg) dtv%id

    if (iostat /= 0) return

    select type (dtv)
        type is (base)

        type is (child)
            write (unit, '(a,a)', iostat=iostat, iomsg=iomsg) ', ', dtv%name
        class default
            error stop 10_4
    end select
end subroutine
