! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc005a20_1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (level-4 expression as the
!                               source-expr; use defined operation for a derived
!                               type)
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
        integer, kind :: k1
        real(k1)      :: data
    end type

    interface operator (.eq.)
        elemental logical function b1EqB2 (b1, b2)
        import base
            class (base(8)), intent(in) :: b1, b2
        end function
    end interface
end module


elemental logical function b1EqB2 (b1, b2)
use m, only : base
    class (base(8)), intent(in) :: b1, b2

    real(8), parameter :: torlerance = 1.0d-15

    real (8) :: diff, avg

    diff = dabs (b1%data - b2%data)

    avg = (b1%data + b2%data) / 2.0d0

    b1EqB2 = ((diff / avg) <= torlerance)
end function


program falloc005a20_1
use m
    type (base(8)) :: b1(10)

    logical, pointer :: l1(:)

    b1%data = (/(i*1.1d0, i=1,10)/)

    allocate (l1(size(b1)), source=(base(8)(2*1.1d0) == b1))

    if ((.not. l1(2)) .or. l1(1) .or. (any (l1(3:)))) error stop 1_4
end
