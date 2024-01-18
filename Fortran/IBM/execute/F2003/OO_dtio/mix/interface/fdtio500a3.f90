! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO on generics (two derived types with DTIO
!                               defined for both of them used in the same I/O
!                               data transfer statement)
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
        integer(4), pointer :: i
    end type

    integer(4), parameter :: ISNULL = -999999

    type base1
        character(10), allocatable :: c
    end type

    character(*), parameter :: UNALLOCATED = 'UNALLOCATE'
end module


program fdtio500a3
use m
    interface read(unformatted)
        subroutine unformattedReadBase (dtv, unit, iostat, iomsg)
        import base
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine unformattedReadBase1 (dtv, unit, iostat, iomsg)
        import base1
            class (base1), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(unformatted)
        subroutine unformattedWriteBase (dtv, unit, iostat, iomsg)
        import base
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine unformattedWriteBase1 (dtv, unit, iostat, iomsg)
        import base1
            class (base1), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    type(base) :: b1
    type (base1) :: b2
    class (base), allocatable :: b3(:)
    class (base1), allocatable :: b4(:)

    integer stat
    character(200) :: errormsg

    integer(4), target :: i1 = 1000

    nullify (b1%i)
    allocate (b2%c, source='xlftest   ')

    open (1, file='fdtio500a3.data', form='unformatted')

    stat = -10
    errormsg = 'no error'

    write (1, iostat=stat, iomsg=errormsg) b1, b2

    if ((stat /= 0) .or. (errormsg /= 'no error')) error stop 1_4

    write (1, iostat=stat, iomsg=errormsg)  base1(null()), base(i1)

    rewind 1

    allocate (b3(2), b4(2))

    read (1, iostat=stat, iomsg=errormsg) b3(1), b4(1)

    if ((stat /= 0) .or. (errormsg /= 'no error')) error stop 2_4

    read (1, iostat=stat, iomsg=errormsg) b4(2), b3(2)

    if ((stat /= 0) .or. (errormsg /= 'no error')) error stop 3_4

    if (associated (b3(1)%i)) error stop 4_4

    if (allocated (b4(2)%c)) error stop 5_4

    if ((b3(2)%i /= 1000) .or. (b4(1)%c /= 'xlftest')) error stop 6_4
end


subroutine unformattedReadBase (dtv, unit, iostat, iomsg)
use m
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) temp, err

    !! we do not care about the failure of deallocation
    if (associated (dtv%i))   deallocate (dtv%i, stat=err)

    read (unit, iostat=iostat, iomsg=iomsg) temp

    if (iostat /= 0) return

    !! check for NULL values
    if (temp == ISNULL) then
        nullify (dtv%i)
    else
        allocate (dtv%i, source=temp)
    end if
end subroutine


subroutine unformattedWriteBase (dtv, unit, iostat, iomsg)
use m
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (associated (dtv%i)) then
        write (unit, iostat=iostat, iomsg=iomsg) dtv%i
    else
        write (unit, iostat=iostat, iomsg=iomsg) ISNULL
    end if
end subroutine


subroutine unformattedReadBase1 (dtv, unit, iostat, iomsg)
use m
    class (base1), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer error
    character(10) temp

    !! we don't care if deallocation fails
    if (allocated (dtv%c)) deallocate (dtv%c, stat=error)

    read (unit, iostat=iostat, iomsg=iomsg) temp

    if (iostat /= 0) return

    if (temp /= UNALLOCATED) allocate (dtv%c, source=temp)
end subroutine


subroutine unformattedWriteBase1 (dtv, unit, iostat, iomsg)
use m
    class (base1), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated(dtv%c)) then
        write (unit, iostat=iostat, iomsg=iomsg) dtv%c
    else
        write (unit, iostat=iostat, iomsg=iomsg) UNALLOCATED
    end if
end subroutine

