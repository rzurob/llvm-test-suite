!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio508.f
! %VERIFY: fdtio508.out:fdtio508.vf
! %STDIN:
! %STDOUT: fdtio508.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/2/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO on generics (for EXTENDS keyword, basic
!                               test)
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
        integer(4) :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name
        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fdtio508
use m
    class (base), pointer :: b1, b2

    integer stat
    character(200) error

    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    call openFile (1, 'fdtio508.data', 'unformatted')

    call writeData

    allocate (b1)
    allocate (child:: b2)

    rewind(1)

    error = 'no error'

    read (1, iostat=stat, iomsg=error) b1, b2

    if (stat /= 0) error stop 2_4

    call b1%print
    call b2%print

    close(1, status='delete')
end


subroutine openFile (i, name, form)
    integer(4), intent(in) :: i
    character(*), intent(in) :: name, form

    open (i, file=name, form=form)
end subroutine

subroutine writeData
use m
    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    integer stat
    character(200) error

    write (1, iostat=stat, iomsg=error) base(10), child(20,'xlftest')

    if (stat /= 0) error stop 1_4
end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    select type (dtv)
        type is (base)
            write (unit, iostat=iostat, iomsg=iomsg) dtv
        type is (child)
            write (unit, iostat=iostat, iomsg=iomsg) dtv
        class default
            error stop 10_4
    end select
end subroutine


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    select type (dtv)
        type is (base)
            read (unit, iostat=iostat, iomsg=iomsg) dtv

        type is (child)
            read (unit, iostat=iostat, iomsg=iomsg) dtv
        class default
            error stop 15_4
    end select
end subroutine
