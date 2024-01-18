! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/3/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO on generics (test of ERR= in parent data
!                               transfer statement, when iostat is nonzero
!                               execution transferred to labeled statement)
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
        character(150), allocatable :: name
    end type
end module

program fdtio507a2
use m
    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    class (base), allocatable :: b1(:)
    integer stat
    character(200) err

    err = 'no error'
    allocate (b1(2))

    open (1, file='fdtio507a2.data', form='unformatted')

    write (1, iostat=stat, iomsg=err) base('xlftest'), base(null())

    if (stat /= 0) error stop 1_4

    backspace (1)

    read (1, iostat=stat, iomsg=err, err=100) b1(1), b1(2)

    print *, stat, err
    print *, b1(1)%name, b1(2)%name
    stop 1

100 rewind (1)


    close (1, status='delete')
end


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(150) name

    integer stat1

    read (unit, iostat=iostat, iomsg=iomsg) name

    if (iostat /= 0) return

    deallocate (dtv%name, stat=stat1)
    allocate (dtv%name, source=name)

end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%name)) then
        write (unit, iostat=iostat, iomsg=iomsg) dtv%name
    else
        write (unit, iostat=iostat, iomsg=iomsg) 'U'
    end if
end subroutine
