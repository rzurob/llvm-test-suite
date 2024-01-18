! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/decimaledit/dtio/dcmlChildInquire001.f
! opt variations: -qck -qnol

! SCCS ID Information
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id
        character(:), allocatable :: name

        contains

        procedure :: writeBaseUfmtd
        generic :: write(unformatted) => writeBaseUfmtd
    end type


    contains

    subroutine writeBaseUfmtd (dtv, unit, iostat, iomsg)
        class(base(*,4)), intent(in) :: dtv
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
    write (1) (i, base(20,4)(i, 'xlftest'), i=1, 100)
end
