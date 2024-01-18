! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (syntax and semantic check for
!                               formatted READ DTIO procedure)
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
        real(4), allocatable :: d1
    end type

    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(2:)  !<-- this is legal
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

program fdtio001
use m
    type (base) :: b1

    integer stat
    character(200) err

    logical precision_r4

    open (1, file='fdtio001.data')

    write (1, '(f10.3)') 10.3

    rewind 1

    read (1, *, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err

        error stop 1_4
    end if

    !! check the value of b1

    if (.not. allocated (b1%d1)) error stop 2_4

    if (.not. precision_r4(b1%d1, 10.3)) error stop 3_4

    close (1, status='delete')
end

subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(2:)  !<-- this is legal
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    real(4) temp

    read (unit, *, iostat=iostat, iomsg=iomsg) temp

    if (iostat /= 0) return

    if (allocated (dtv%d1))  deallocate (dtv%d1)

    allocate (dtv%d1, source=temp)
end subroutine

