! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc507.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc507.f
! %VERIFY: falloc507.out:falloc507.vf
! %STDIN:
! %STDOUT: falloc507.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (internal subroutine named as
!                               ALLOCATE)
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
    type base(k1)    ! (8)
        integer, kind         :: k1
        real(k1), allocatable :: data(:)

        contains

        procedure :: verify => validateData
        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1,k3)    ! (8,1,20,8)
        integer, kind             :: k2,k3
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
        integer(k3)               :: id

        contains

        procedure :: print => printChild
    end type

    contains

    logical function validateData (b, data)
        class (base(8)), intent(in) :: b
        real(8), intent(in) :: data(:)

        logical precision_r8

        if (.not. allocated (b%data)) then
            validateData = .false.
            return
        else
            validateData = (size (data) == size (b%data))

            if (validateData) then
                do i = 1, size (data)
                    if (.not. precision_r8 (data(i), b%data(i))) then
                        validateData = .false.
                        return
                    end if
                end do
            end if
        end if
    end function

    subroutine printBase (b)
        class (base(8)), intent(in) :: b

        if (allocated (b%data)) print *, 'sizeof data is ', size(b%data)
    end subroutine

    subroutine printChild (b)
        class (child(8,1,*,8)), intent(in) :: b

        call b%base%print

        print *, b%name, b%id
    end subroutine
end module

program falloc507
use m
    class (base(8)), allocatable :: b1, b2, b3, b4

    type (base(8)) b11

    real(8) :: r1 (6)

    call allocate (b1, b2, b3, b4, source=child(8,1,20,8)((/1.0,2.0/), 'test', 10))

    call b1%print
    call b2%print
    call b3%print
    call b4%print

    if (.not. b1%verify ((/1.00d0, .2d1/))) error stop 1_4

    if (b2%verify ((/.1d1, .201d1/))) error stop 2_4

    if (.not. b3%verify (b1%data)) error stop 3_4

    if (.not. b4%verify ((/b2%data(1), b3%data(2)/))) error stop 4_4


    b11 = base(8) ((/b1%data, b2%data(2:1:-1), b3%data+b4%data/))

    r1 = (/1.d0, 2.d0, 2.d0, 1.d0, 2.d0, 4.d0/)

    deallocate (b1, b2, b3, b4)

    call allocate (b1, b2, b3, b4, source=b11)

    if (.not. b1%verify (data = r1)) error stop 5_4
    if (.not. b2%verify (data = b1%data)) error stop 6_4
    if (.not. b3%verify (r1)) error stop 7_4
    if (.not. b4%verify (r1)) error stop 8_4

    call b3%print

    contains

    subroutine allocate (bb1, bb2, bb3, bb4, source)
        class (base(8)), allocatable :: bb1, bb2, bb3, bb4
        class (base(8)), intent(in) :: source

        allocate (bb1, source=source)
        allocate (bb2, source=source)
        allocate (bb3, source=source)
        allocate (bb4, source=source)
    end subroutine
end
