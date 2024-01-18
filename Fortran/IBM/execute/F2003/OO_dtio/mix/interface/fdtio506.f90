!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/03/2005
!*
!*  DESCRIPTION                : DTIO generics (DTIO calls during DTIO)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(4), pointer :: i => null()
    end type

    integer(4), parameter :: ISNULL = -999999

    type container
        class (base), allocatable :: data
    end type

    integer(4), parameter :: ISALLOCATED = 1
    integer(4), parameter :: UNALLOCATED = 0
end module

module m1
use m
    interface read(unformatted)
        subroutine unformattedReadBase (dtv, unit, iostat, iomsg)
        import base
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine unformattedReadContainer (dtv, unit, iostat, iomsg)
        import container
            class (container), intent(inout) :: dtv
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

        subroutine unformattedWriteContainer (dtv, unit, iostat, iomsg)
        import container
            class (container), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


program fdtio506
use m1
    integer stat
    character(200) :: errormsg

    integer(4), target :: i1 = 200

    type (container) co1(3)

    allocate (co1(2)%data)
    allocate (co1(2)%data%i, source=100)

    allocate (co1(3)%data, source=base(null()))

    open (1, file='fdtio506.data', form='unformatted')

    write (1, iostat=stat, iomsg=errormsg) co1

    if (stat /= 0) then
        print *, stat, errormsg
        error stop 1_4
    end if

    rewind 1

    read (1, iostat=stat, iomsg=errormsg) co1(3:1:-1)

    if (stat /= 0) then
        print *, stat, errormsg
        error stop 2_4
    end if

    if ((.not. allocated (co1(1)%data)) .or. (.not. allocated (co1(2)%data)) &
           .or. allocated (co1(3)%data)) error stop 3_4

    if (associated (co1(1)%data%i)) error stop 4_4

    if (.not. associated (co1(2)%data%i)) error stop 5_4

    if (co1(2)%data%i /= 100) error stop 6_4

    close(1, status='delete')
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

    !! check for NULL
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


subroutine unformattedWriteContainer (dtv, unit, iostat, iomsg)
use m
use m1, only: write(unformatted)
    class (container), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    !! we use an extra field to test if the allocatable component is allocated
    if (.not. allocated (dtv%data)) then
        write (unit, iostat=iostat, iomsg=iomsg) unallocated, base(null())
    else
        write (unit, iostat=iostat, iomsg=iomsg) isallocated, dtv%data
    end if
end subroutine


subroutine unformattedReadContainer (dtv, unit, iostat, iomsg)
use m
use m1, only: read(unformatted)
    class (container), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) flag

    type (base) temp

    if (allocated (dtv%data)) deallocate (dtv%data)

    !! read in the two field
    read (unit, iostat=iostat, iomsg=iomsg) flag, temp

    if (iostat /= 0) return

    if (flag == isallocated) then
        allocate (dtv%data, source=temp)
    end if
end subroutine
