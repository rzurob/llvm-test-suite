!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/30/2005
!*
!*  DESCRIPTION                : DTIO generics (formatted-directed write with NO
!                               DT edit descripts; runtime error expected)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), private :: data(2)

        contains

        procedure :: print => printBase
        procedure :: setVal => updateBase
        procedure :: getVal => getBaseVal
    end type


    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface


    contains

    subroutine updateBase (b, val)
        class (base), intent(inout) :: b
        real(8), intent(in) :: val(2)

        b%data = val
    end subroutine

    real(8) function getBaseVal (b)
        class (base), intent(in) :: b
        dimension getBaseVal(2)

        getBaseVal = b%data
    end function


    subroutine printBase (b, unit, iostat, iomsg)
        class (base), intent(in) :: b
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        write (unit, '(10x, 2g10.2,/)', iostat=iostat, iomsg=iomsg) b%getVal()
    end subroutine
end module


subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') stop 11

    if (size (v_list) /= 0) error stop 10_4

    call dtv%print(unit, iostat, iomsg)
end subroutine

program fdtio519b
use m
    class (base), allocatable :: b1(:)

    integer stat1
    character(200) err

    allocate (b1(2))


    call b1(1)%setVal ((/1.3_8, 2.1_8/))
    call b1(2)%setVal ((/-2.4_8, -3.2_8/))

    write (*, '(2f10.2)', iostat=stat1, iomsg=err) b1(1)

    if (stat1 == 0) then
        print *, stat1, err
        error stop 1_4
    end if

    write (*, '(2(2f10.2))', iostat=stat1, iomsg=err) b1

    if (stat1 == 0) then
        print *, stat1, err
        error stop 2_4
    end if

    write (*, '(2f10.2)', iostat=stat1, iomsg=err) 10.2, b1(1)
end
