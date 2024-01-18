!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio514a_1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/06/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (with pad='yes', slash edit
!                               descriptor will force the read start a new
!                               record)
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
        character(10), allocatable :: name

        contains

        procedure :: read => readBase
    end type

    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    contains

    subroutine readBase (b, unit, iotype, v_list, iostat, iomsg)
        class (base), intent(inout) :: b
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (allocated (b%name)) deallocate (b%name)

        allocate (b%name)

        read (unit, '(a10/)', iostat=iostat, iomsg=iomsg) b%name
    end subroutine
end module


subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    call dtv%read (unit, iotype, v_list, iostat, iomsg)
end subroutine


program fdtio514a
use m
use iso_fortran_env

    integer stat1
    character(200) err

    class (base), allocatable :: b1(:)

    allocate (b1(2))

    open (1, file='fdtio514a_1.data', pad='yes')

    write (1, '(a)') 'abc, efg, xyz'

    rewind (1)

    read (1, *, iostat=stat1, iomsg=err) b1(1), b1(2)


    if (stat1 /= iostat_end) then
        print *, stat1, err
        error stop 1_4
    end if

    if ((.not. allocated (b1(1)%name)) .or. (.not. allocated (b1(2)%name))) &
            error stop 2_4


    if (b1(1)%name /= 'abc, efg, ') error stop 3_4

    close (1, status='delete')
end

