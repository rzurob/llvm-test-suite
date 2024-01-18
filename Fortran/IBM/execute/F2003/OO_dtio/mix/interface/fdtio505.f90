! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO on generics (DTIO on sub-objects)
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

    type container
        type(base) :: data
    end type
end module


program fdtio505
use m

    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        import base
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        import base
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    type (container) :: co1, co2

    integer stat
    character(200) :: errormsg = ''

    integer(4), target :: i1 = 200

    open (1, file='fdtio505.data', form='unformatted')

    write (1, iostat=stat, iomsg=errormsg)  container(base(i1))

    if ((stat /= 0) .or. (errormsg /= '')) error stop 1_4

    write (1, iostat=stat, iomsg=errormsg)  container (base(null()))

    if ((stat /= 0) .or. (errormsg /= '')) error stop 2_4

    rewind 1

    read (1, iostat=stat, iomsg=errormsg) co1

    if ((stat /= 0) .or. (errormsg /= '')) error stop 3_4

    read (1, iostat=stat, iomsg=errormsg) co2

    if ((stat /= 0) .or. (errormsg /= '')) error stop 4_4

    !! verify the data read in: co1 and co2
    if (associated (co2%data%i)) error stop 5_4

    if (co1%data%i /= i1) error stop 6_4
end


subroutine unformattedRead (dtv, unit, iostat, iomsg)
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

    !! check for NULL
    if (temp == ISNULL) then
        nullify (dtv%i)
    else
        allocate (dtv%i, source=temp)
    end if
end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
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

