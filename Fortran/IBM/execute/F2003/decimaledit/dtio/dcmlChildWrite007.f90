!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/09/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Use of decimal= and DC/DP in child write
!                               statement.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
        complex(8) cx
        character(:), allocatable :: name
    end type

    character(*), parameter :: modes(2) = (/'POINT', 'COMMA'/)

    interface write(formatted)
        module procedure writeBaseFmtd
    end interface

    contains

    !! this subroutine reverse the decimal mode in the parent for writing the
    ! real part of dtv%cx
    subroutine writeBaseFmtd(dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        write (unit, '(dp, i6,1x)') dtv%id

        write (unit, '(d18.10, 1x)', decimal=modes(queryFileMode(unit))) &
                real(dtv%cx)

        write (unit, '(d18.10, 1x, a)') aimag(dtv%cx), dtv%name
    end subroutine

    integer function queryFileMode (unit)
        integer, intent(in) :: unit

        character(:), allocatable :: parentDecMode

        parentDecMode = repeat(' ', 5)

        inquire (unit, decimal=parentDecMode)

        if (parentDecMode == modes(1)) then
            queryFileMode = 2
        else if (parentDecMode == modes(2)) then
            queryFileMode= 1
        else
            error stop 10_4
        end if
    end function
end module

program dcmlChildWrite007
use m
    type(base) b1(10)

    b1 = (/(base(i, cmplx(i, 2*i, 8), genName(i)), i=1, 10)/)

    open (1, file='dcmlChildWrite007.out', decimal='Comma')

    write (1, '(10DT)') b1

    write (1, *, decimal='Point') b1

    write (1, '(5(DP, dt, dc, dt))') b1

    close(1)

    contains

    function genName (i)
        character(i) genName

        genName = 'xlftest team members'
    end function
end
