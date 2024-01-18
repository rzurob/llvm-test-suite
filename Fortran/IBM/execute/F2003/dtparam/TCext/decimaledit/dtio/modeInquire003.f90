! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/decimaledit/dtio/modeInquire003.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! SCCS ID Information
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure :: readBaseUFmtd
        generic :: read(unformatted) => readBaseUFmtd
    end type

    character(:), allocatable :: modeString

    contains

    subroutine readBaseUFmtd (dtv, unit, iostat, iomsg)
        class(base(4,*)), intent(inout) :: dtv
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
    type A(k2,n2)    ! (4,20)
        integer, kind              :: k2
        integer, len               :: n2
        class(base(k2,:)), pointer :: data => null()
    end type

    interface read(unformatted)
        module procedure readAUFmtd
    end interface

    contains

    subroutine readAUFmtd (dtv, unit, iostat, iomsg)
        class(A(4,*)), intent(inout) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. associated(dtv%data)) allocate(base(4,dtv%n2) :: dtv%data)

        read (unit, iostat=iostat, iomsg=iomsg) dtv%data
    end subroutine
end module

program modeInquire003
use n
    type(base(4,20)) b1

    type(A(4,:)), allocatable :: a1(:)

    allocate (A(4,20) :: a1(1000))

    write (1) 1.0
    write (1) 1.0

    rewind 1

    read (1) b1

    read (1) a1

    if (modeString /= repeat('UNDEFINED ', 1001)) error stop 1_4
end
