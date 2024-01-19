! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/07/2005
!*
!*  DESCRIPTION                : DTIO generics (subroutine calls during the DTIO
!                               procedure)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(4), allocatable :: i1
    end type

    type, extends(base) :: child
        real(4), allocatable :: r1
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

    contains

    subroutine readUBase (dtv, unit, iostat, iomsg)
        type (base), intent(inout) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. allocated (dtv%i1)) allocate (dtv%i1)

        read (unit, iostat=iostat, iomsg=iomsg) dtv%i1
    end subroutine

    subroutine readUChild (dtv, unit, iostat, iomsg)
        type (child), intent(inout) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. allocated (dtv%i1)) allocate (dtv%i1)
        if (.not. allocated (dtv%r1)) allocate (dtv%r1)

        read (unit, iostat=iostat, iomsg=iomsg) dtv%i1, dtv%r1
    end subroutine
end module


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m, only: base, child, readUBase, readUChild
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    select type (dtv)
        type is (base)
            call readUBase (dtv, unit, iostat, iomsg)
        type is (child)
            call readUChild (dtv, unit, iostat, iomsg)
        class default
            error stop 20_4
    end select
end subroutine

program fdtio510a2
use m
    class (base), allocatable :: b1, b2

    integer stat
    character(200) err

    logical(4) precision_r4

    allocate (b1)
    allocate (child:: b2)

    write (1) 200, 2.1
    write (1) 300

    rewind 1

    read (1, iostat=stat, iomsg=err) b2

    if (stat /= 0) then
        print *, stat, err
        error stop 2_4
    end if

    select type (b2)
        type is (child)
            if (b2%i1/= 200) error stop 3_4

            if (.not. precision_r4 (b2%r1, 2.1_4)) error stop 4_4
        class default
            error stop 5_4
    end select

    read (1, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err
        error stop 6_4
    end if

    if (b1%i1 /= 300) error stop 7_4

    close(1, status='delete')
end
