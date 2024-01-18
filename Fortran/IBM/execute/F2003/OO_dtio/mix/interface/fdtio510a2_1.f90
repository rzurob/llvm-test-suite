!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio510a2_1.f
! %VERIFY: fdtio510a2_1.out:fdtio510a2_1.vf
! %STDIN:
! %STDOUT: fdtio510a2_1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (one DTIO subroutine calls
!                               different subroutines based on the dynamic type
!                               of dtv)
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
        integer(4), allocatable :: i1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        real(4), allocatable :: r1

        contains

        procedure :: print => printChild
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

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%i1
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print '(i5,f10.2)', b%i1, b%r1
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
            error stop 10_4
    end select
end subroutine

program fdtio510a2_1
use m
    class (base), allocatable :: b1, b2

    integer stat
    character(200) err

    allocate (b1)
    allocate (child:: b2)

    write (1) 100, 200, 2.1

    rewind 1

    read (1, iostat=stat, iomsg=err) b1, b2

    if (stat /= 0) then
        print *, stat, err
        error stop 1_4
    end if

    call b1%print
    call b2%print
end
