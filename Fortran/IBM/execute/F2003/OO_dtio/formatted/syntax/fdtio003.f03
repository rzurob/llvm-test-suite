! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (test the dtv; functional test
!                               that DTIO defined for base type will be used for
!                               the data declared of child type)
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
        character(5), allocatable :: data
    end type

    type, extends(base) :: child
        integer(4) :: i1
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

program fdtio003
use m
    class (child), allocatable :: c1
    class (base), allocatable :: b1
    type (child) c2

    allocate (c1, source=child('abcde', 100))
    allocate (b1, source=child('ABCDE', 200))

    allocate (c2%data, source='XYZ  ')
    c2%i1 = 300

    print *, c1

    write (*,*) b1

    print *, c2
end

subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%data)) &
            write (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
end subroutine
