! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/11/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (unformatted sequential read that
!                               results in EOF condition)
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
        class (*), allocatable :: data(:)
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


!! this subroutine tries to read in data as integer(8) and an array of 2
!elements
subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m, only: base
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%data)) deallocate (dtv%data)

    allocate (integer(8) :: dtv%data(2))

    select type (x => dtv%data)
        type is (integer(8))
            read (unit, iostat=iostat, iomsg=iomsg) x
        class default
            error stop 10_4
    end select
end subroutine


program fdtio514a4
use m
use iso_fortran_env
    class (base), pointer :: b1(:)

    integer stat1
    character(200) err

    allocate (b1(3))

    write (1) 1_8, 10_8

    rewind(1)

    !! first read will succeed
    read (1, iostat=stat1, iomsg=err) b1(1)

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 1_4
    end if

    !! the 2nd read stament will result in EOF
    read (1, iostat=stat1, iomsg=err) b1(2:3)

    if (stat1 /= iostat_end) then
        print *, stat1, err
        error stop 2_4
    end if


    !! verify that the 1st element is read in correctedly
    select type (x => b1(1)%data)
        type is (integer(8))
            if (size(x) /= 2) error stop 3_4

            if (any (x /= (/1_8, 10_8/))) error stop 4_4
        class default
            error stop 5_4
    end select

    close (1, status='delete')
end
