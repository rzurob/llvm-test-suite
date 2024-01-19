! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp -qreuse=base /tstdev/F2003/decimaledit/defaultIO/dcmlCharExprRW003a.f
! opt variations: -qnock -qnol -qnodeferredlp -qreuse=self -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/07/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Use of the namelist for the format control with
!                               DECIMAL= specifier; test read as well; use
!                               stream access file.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1,k2)    ! (20,4,4)
        integer, kind :: k1,k2
        integer, len  :: n1
        integer(k1)   :: id
        real(k2)      :: d(2)
    end type

    type, extends(base) :: child(k3)    ! (20,4,4,1)
        integer, kind             :: k3
        character(kind=k3,len=n1) :: name
        complex(k1)               :: cx
    end type
end module


program dcmlCharExprRW003a
use m
    type(base(:,4,4)), allocatable :: b1(:), b2(:)

    type(child(20,4,4,1)) c1, c2

    integer pos

    abstract interface
        subroutine operateIO (unit, pos, b1, c1)
        import
            integer, intent(in) :: unit, pos
            type(base(*,4,4)), intent(out) :: b1(10)
            type(child(*,4,4,1)), intent(out) :: c1
        end subroutine
    end interface

    procedure (operateIO) readWithPointMode, readWithCommaMode
    namelist /nml1/ b1, c1

    allocate (b1(10), source=(/(base(20,4,4)(i, (/i, i*2/)), i=1,10)/))
    allocate (base(20,4,4) :: b2(0:9))

    c1 = child(20,4,4,1) (100, 1.5, 'xlftest F2003', cmplx(-1, -10))

    open (1, file='dcmlCharExprRW003.data', form='formatted', access='stream',&
            decimal='COMMA', delim='APOSTROPHE')

    write (1, nml1, decimal="POINT", pos=100)

    write (1, nml1)

    call readWithPointMode (1, 100, b2, c2)

    !! verify b2, c2
    call verifyData

    deallocate (b2)

    allocate (base(20,4,4) :: b2(0:9))

    inquire (1, pos=pos)

    call readWithCommaMode (1, pos, b2, c2)

    call verifyData

    contains

    !! the following subroutine verifies that b1 == b2 and c1 == c2
    subroutine verifyData
        logical(4), external :: precision_r4, precision_x8

        do i = 0, 9
            if (b2(i)%id /= i+1) error stop 1_4

            if (.not. precision_r4(b2(i)%d(1), i*1.0+1)) error stop 2_4
            if (.not. precision_r4(b2(i)%d(2), i*2.0+2)) error stop 3_4

            if (c2%id /= 100) error stop 4_4

            if ((.not. precision_r4(c2%d(1), 1.5)) .or. &
                (.not. precision_r4(c2%d(2), 1.5))) error stop 5_4

            if (c2%name /= 'xlftest F2003') error stop 6_4

            if (.not. precision_x8(c2%cx, cmplx(-1, -10))) error stop 7_4
        end do
    end subroutine
end

subroutine readWithPointMode (unit, pos, b1, c1)
use m
    integer, intent(in) :: unit, pos
    type(base(*,4,4)), intent(out) :: b1(10)
    type(child(*,4,4,1)), intent(out) :: c1

    namelist /nml1/ b1, c1

    read(unit, nml1, pos=pos, decimal="POINT")
end subroutine

subroutine readWithCommaMode (unit, pos, b1, c1)
use m
    integer, intent(in) :: unit, pos
    type(base(*,4,4)), intent(out) :: b1(10)
    type(child(*,4,4,1)), intent(out) :: c1

    namelist /nml1/ b1, c1

    read(unit, nml1, pos=pos)
end subroutine
