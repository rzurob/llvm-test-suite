! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (test unit value; for internal
!                               file the unit value is negative values)
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

    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(len(dtv%data)) temp

    !! test that unit is negative value

    if (unit >= 0) then
        iostat=unit
        iomsg = 'negative value is expected'
        return
    end if

    read (unit, *, iostat=iostat, iomsg=iomsg) temp

    if (iostat /= 0) return

    if (allocated (dtv%data)) deallocate (dtv%data)

    allocate (dtv%data, source=temp)
end subroutine

program fdtio002
use m
    class (base), pointer :: b1

    integer stat
    character(200) err

    character(10) :: c1 = 'abcdefghij'

    allocate (b1)

    read(c1, *, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err
        error stop 1_4
    end if

    !! verify the read in data
    if (.not. allocated (b1%data)) error stop 2_4

    if (b1%data /= 'abcde') error stop 3_4
end
