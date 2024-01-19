! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet001.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : poly-function-return (a basic test)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    contains

    class (base(4)) function produceBasePtr (i)
        integer*4, intent(in) :: i
        pointer produceBasePtr

        allocate (produceBasePtr, source=base(4)(i))
    end function

    class (*) function produceAnything (x)
        class (*), intent(in) :: x
        pointer produceAnything

        allocate (produceAnything, source=x)
    end function
end module

program ffuncRet001
use m
    type seq1(k2,k3,k4)    ! (4,2,8)
        integer, kind :: k2,k3,k4
        sequence
        integer(k2)      i1
        integer(k3)      i2
        integer(k4)      i3
    end type

    class (base(4)), pointer :: b1
    type (base(4)), pointer :: b2
    type (seq1(4,2,8)), pointer :: s1

    b1 => produceBasePtr (10)

    b2 => produceBasePtr (20)

    s1 => produceAnything (seq1(4,2,8) (1, 10, 100))

    if ((b1%id /= 10) .or. (b2%id /= 20)) error stop 1_4

    if ((s1%i1 /= 1) .or. (s1%i2 /= 10) .or. (s1%i3 /= 100)) error stop 2_4

    deallocate (b2, b1, s1)
end
