!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/08/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that inquire statement in unformatted child
!                               write statement returns UNDEFINED.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
        character(:), allocatable :: name

        contains

        procedure :: writeBaseUfmtd
        generic :: write(unformatted) => writeBaseUfmtd
    end type


    contains

    subroutine writeBaseUfmtd (dtv, unit, iostat, iomsg)
        class(base), intent(in) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(:), allocatable :: mode

        allocate (character(15) :: mode)

        inquire(unit, decimal=mode)

        if (mode /= 'UNDEFINED') error stop 1_4

        mode = 'comma'

        inquire(unit, decimal=mode)

        if (mode /= 'UNDEF') error stop 2_4
    end subroutine
end module

program dcmlChildInquire001
use m
    write (1) (i, base(i, 'xlftest'), i=1, 100)
end
