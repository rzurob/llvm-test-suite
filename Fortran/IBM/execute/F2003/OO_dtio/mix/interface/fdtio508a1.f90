!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/31/2005
!*
!*  DESCRIPTION                : DTIO generics (test the unformatted read in
!                               select type construct)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: id(:)
    end type

    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        import base
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m, only: base
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer size1

    if (allocated (dtv%id)) deallocate (dtv%id)

    !! read size and then the array
    read (unit, iostat=iostat, iomsg=iomsg) size1

    if (iostat /= 0) return

    if (size1 >= 0) then
        allocate (dtv%id(size1))

        read (unit, iostat=iostat, iomsg=iomsg) dtv%id
    end if
end subroutine


program fdtio508a1
use m
    integer stat1
    character(200) err

    type container
        class (*), pointer :: data(:)
    end type

    type (container) :: co1

    open (1, file='fdtio508a1.data', form='unformatted')

    write (1) 2, 10, 15

    write (1) 3, 15, 5, 3, 2, 2, 3

    rewind (1)

    allocate (base:: co1%data(2))

    select type (y => co1%data)
        class is (base)
            !! test the read for scalar
            read (1, iostat=stat1, iomsg=err) y(1)

            if (stat1 /= 0) then
                print *, stat1, err
                error stop 5_4
            end if

            if (size (y(1)%id) /= 2) error stop 6_4

            if (any (y(1)%id /= (/10, 15/))) error stop 7_4

            !! test read for array
            read (1, iostat=stat1, iomsg=err) y

            if (stat1 /= 0) then
                print *, stat1, err
                error stop 8_4
            end if

            if ((size (y(1)%id) /= 3) .or. (size (y(2)%id) /= 2)) error stop 9_4

            if (any (y(1)%id /= (/15,5,3/))) error stop 10_4

            if (any (y(2)%id /= (/2,3/))) error stop 11_4
        class default
            error stop 1_4
    end select

    close (1, status='delete')
end
