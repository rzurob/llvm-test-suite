!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio508a.f
! %VERIFY: fdtio508a.out:fdtio508a.vf
! %STDIN:
! %STDOUT: fdtio508a.out
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
!*  DESCRIPTION                : DTIO on generics (extended types in the DTIO
!                               routines; use select type construct during the
!                               child data transfer; test arrays)
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

program fdtio508a
use m
    class (base), pointer :: b1(:), b2(:)

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

    call openFile (1, 'fdtio508a.data', 'unformatted')

    call writeData

    allocate (b1(2))
    allocate (child:: b2(2))

    rewind(1)

    error = 'no error'

    read (1, iostat=stat, iomsg=error) b1, b2

    if (stat /= 0) error stop 2_4

    call b1(1)%print
    call b1(2)%print

    call b2(1)%print
    call b2(2)%print

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

    class (base), allocatable :: b1(:), b2(:)

    allocate (b1(0:1), source=(/base(10), base(20)/))
    allocate (b2(-1:0), source=(/child(100, 'xlftest A'), child(200, 'xlftest B') &
                /))


    write (1, iostat=stat, iomsg=error) b1, b2

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
