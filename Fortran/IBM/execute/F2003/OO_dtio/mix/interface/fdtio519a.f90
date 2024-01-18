!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio519a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/17/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (list-directed parent write for
!                               derived type with private component)
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
        integer, private :: data (2)

        contains

        procedure :: read => readBase
        procedure :: getVal => getBaseVal
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

    subroutine readBase (b, unit, iostat, iomsg)
        class (base), intent (inout) :: b
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        read (unit, *, iostat=iostat, iomsg=iomsg) b%data
    end subroutine

    integer function getBaseVal (b)
        class (base), intent(in) :: b
        dimension getBaseVal (2)

        getBaseVal = b%data
    end function
end module


subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only : base
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size (v_list) /= 0) error stop 10_4

    call dtv%read (unit, iostat=iostat, iomsg=iomsg)
end subroutine


program fdtio519a
use m
    class (base), allocatable :: b1(:,:)

    integer stat1
    character(200) err

    allocate (b1(2,1))

    write (1, '(5i10)') 10, 3, 1, 5, 7
    write (1, '(4i10)') 2, 3, 5, 6

    rewind 1

    !! test read for scalar
    read (1, *, iostat=stat1, iomsg=err) b1(1,1)

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 1_4
    end if

    if (any(b1(1,1)%getVal() /= (/10, 3/))) error stop 2_4


    read (1, *, iostat=stat1, iomsg=err) b1

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 3_4
    end if

    if (any(b1(1,1)%getVal() /= (/2, 3/))) error stop 4_4
    if (any(b1(2,1)%getVal() /= (/5, 6/))) error stop 5_4

    close (1, status='delete')
end
