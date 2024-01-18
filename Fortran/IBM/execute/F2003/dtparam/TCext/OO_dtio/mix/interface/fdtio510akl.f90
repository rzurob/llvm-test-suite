! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio510akl
!*
!*  DATE                       : 2007-08-14 (original: 11/10/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (test on cases that child data
!                               transfer becomes parent)
!                               adaptation: exposed kind
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
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1), pointer :: i
    end type

    type dataType (kdataType_1) ! kdataType_1=4
       integer, kind :: kdataType_1
        type (base(kdataType_1)), pointer :: data ! tcx: (kdataType_1)
    end type

    type(dataType(4)) :: d1(3) ! tcx: (4)
end module

program fdtio510akl
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
            class (dataType(4)), intent(in) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout):: iomsg
        end subroutine
    end interface

    type (base(4)), target :: b1, b2 ! tcx: (4)
    integer stat
    character (200) :: err

    allocate (b1%i, source=100)
    b2%i => null()

    write (1, iostat=stat, iomsg=err) dataType(4)(b1) ! tcx: (4)

    if (stat /= 0) error stop 101_4

    write (1, iostat=stat, iomsg=err) dataType(4) (b2) ! tcx: (4)

    if (stat /= 0) error stop 2_4

    write (1, iostat=stat, iomsg=err) dataType(4) (null()) ! tcx: (4)

    if (stat /= 0) error stop 3_4

end subroutine


subroutine unformattedWriteBase (dtv, unit, iostat, iomsg)
use m
    class (base(4)), intent(in) :: dtv ! tcx: (4)
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
    class (dataType(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout):: iomsg

    interface write(unformatted)
        subroutine unformattedWriteBase (dtv, unit, iostat, iomsg)
        use m
            class (base(4)), intent(in) :: dtv ! tcx: (4)
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
            class (dataType(4)), intent(inout) :: dtv ! tcx: (4)
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
    class (dataType(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    interface read(unformatted)
        subroutine unformattedReadBase (dtv, unit, iostat, iomsg)
        use m
            class (base(4)), intent(inout) :: dtv ! tcx: (4)
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
    class (base(4)), intent(inout) :: dtv ! tcx: (4)
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


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 6 changes
! type: dataType - added parameters (kdataType_1) to invoke with (4) / declare with (4) - 8 changes
