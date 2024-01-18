! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (DTIO defined for child type can
!                               not be used for data declared to be base type)
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
        character(5) :: data
    end type

    type, extends(base) :: child
        integer(4), allocatable :: i1
    end type

    interface write(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import child
            class (child), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

program fdtio003d2
use m
    class (base), allocatable :: b1

    allocate (b1, source=child('abcde', 100))

    print *, b1  !<-- illegal call
end
