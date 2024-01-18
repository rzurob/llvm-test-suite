!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/24/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that INQUIRE statement on decimal= returns
!                               UNDEFINED if the read is unformatted.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        contains

        procedure :: readBaseUFmtd
        generic :: read(unformatted) => readBaseUFmtd
    end type

    character(:), allocatable :: modeString

    contains

    subroutine readBaseUFmtd (dtv, unit, iostat, iomsg)
        class(base), intent(inout) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(20) mode

        inquire (unit, decimal=mode)

        if (.not. allocated(modeString)) then
            modeString = trim(mode)
        else
            modeString = modeString // ' ' // trim(mode)
        end if
    end subroutine
end module

module n
use m
    type A
        class(base), pointer :: data => null()
    end type

    interface read(unformatted)
        module procedure readAUFmtd
    end interface

    contains

    subroutine readAUFmtd (dtv, unit, iostat, iomsg)
        class(A), intent(inout) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. associated(dtv%data)) allocate(dtv%data)

        read (unit, iostat=iostat, iomsg=iomsg) dtv%data
    end subroutine
end module

program modeInquire003
use n
    type(base) b1

    type(A), allocatable :: a1(:)

    allocate (a1(1000))

    write (1) 1.0
    write (1) 1.0

    rewind 1

    read (1) b1

    read (1) a1

    if (modeString /= repeat('UNDEFINED ', 1001)) error stop 1_4
end
