! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (function return result as the
!                               list item; polymorphic return)
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

    class (base) function produceBasePtr (id, name)
        pointer produceBasePtr
        integer(8), intent(in) :: id
        character(*), optional, intent(in) :: name

        if (present (name)) then
            allocate (produceBasePtr, source=child(id, name))
        else
            allocate (produceBasePtr, source=base(id))
        end if
    end function
end module

program fdtio512
use m

    print *, produceBasePtr (100_8)
    print *, produceBasePtr (200_8, 'xlftest team')
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
            write (unit, *, iostat=iostat, iomsg=iomsg) ', ', dtv%name
        class default
            error stop 10_4
    end select
end subroutine
