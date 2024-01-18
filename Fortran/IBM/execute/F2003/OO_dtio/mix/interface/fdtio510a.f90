!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio510a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO on generics (test on cases that child data
!                               transfer becomes parent)
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

    type dataType
        type (base), pointer :: data
    end type

    type(dataType) :: d1(3)
end module

program fdtio510a
use m
    call writeData

    do i = 1, 2
        call readData

        !! verify the results

        if (d1(1)%data%i /= 100) error stop 20_4

        if ((.not. associated(d1(2)%data)) .or. associated (d1(2)%data%i)) &
                error stop 21_4

        if (associated (d1(3)%data)) error stop 22_4

        print *, i,'iteration completed'
    end do

    close (1, status='delete')
end

subroutine writeData
use m
    interface write(unformatted)
        subroutine unformattedWriteDataType (dtv, unit, iostat, iomsg)
        use m
            class (dataType), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout):: iomsg
        end subroutine
    end interface

    type (base), target :: b1, b2
    integer stat
    character (200) :: err

    allocate (b1%i, source=100)
    b2%i => null()

    write (1, iostat=stat, iomsg=err) dataType(b1)

    if (stat /= 0) error stop 1_4

    write (1, iostat=stat, iomsg=err) dataType (b2)

    if (stat /= 0) error stop 2_4

    write (1, iostat=stat, iomsg=err) dataType (null())

    if (stat /= 0) error stop 3_4

end subroutine


subroutine unformattedWriteBase (dtv, unit, iostat, iomsg)
use m
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout):: iomsg

    if (associated (dtv%i)) then
        write (unit, iostat=iostat, iomsg=iomsg) 'A', dtv%i
    else
        write (unit, iostat=iostat, iomsg=iomsg) 'U'
    end if
end subroutine


subroutine unformattedWriteDataType (dtv, unit, iostat, iomsg)
use m
    class (dataType), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout):: iomsg

    interface write(unformatted)
        subroutine unformattedWriteBase (dtv, unit, iostat, iomsg)
        use m
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout):: iomsg
        end subroutine
    end interface

    if (associated (dtv%data)) then
        write (unit, iostat=iostat, iomsg=iomsg) 'A', dtv%data
    else
        write (unit, iostat=iostat, iomsg=iomsg) 'U'
    end if
end subroutine


subroutine readData
use m
    interface read (unformatted)
        subroutine unformattedReadDataType (dtv, unit, iostat, iomsg)
        use m
            class (dataType), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    integer stat
    character*200 err

    rewind 1

    read (1, iostat=stat, iomsg=err) d1(1)

    if (stat /= 0) error stop 10_4

    read (1, iostat=stat, iomsg=err) d1(2)

    if (stat /= 0) error stop 11_4

    read (1, iostat=stat, iomsg=err) d1(3)

    if (stat /= 0) error stop 12_4
end subroutine


subroutine unformattedReadDataType (dtv, unit, iostat, iomsg)
use m
    class (dataType), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    interface read(unformatted)
        subroutine unformattedReadBase (dtv, unit, iostat, iomsg)
        use m
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    character(1) ptrStat
    integer temp

    read (unit, iostat=iostat, iomsg=iomsg) ptrStat


    if (iostat /= 0) return

    !! only read further if data is associated
    if (ptrStat == 'A') then
        if (associated(dtv%data)) deallocate (dtv%data, stat=temp) !<-- may leak memory
        allocate (dtv%data)


        read (unit, iostat=iostat, iomsg=iomsg) dtv%data
    end if
end subroutine


subroutine unformattedReadBase (dtv, unit, iostat, iomsg)
use m
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(1) ptrStat
    integer temp

    read (unit, iostat=iostat, iomsg=iomsg) ptrStat

    if (iostat /= 0) return

    !! if ptrStat is 'A' then dtv%data will be associated
    if (ptrStat == 'A') then
        if (associated (dtv%i)) deallocate (dtv%i, stat=temp)

        allocate (dtv%i)

        read (unit, iostat=iostat, iomsg=iomsg) dtv%i
    end if
end subroutine
