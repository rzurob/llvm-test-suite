! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_procptr/declaration2/procInterface001e.f
! opt variations: -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The associated
!                              function is an internal function. Poly
!                              and unlimited poly. Intrinsic or derived
!                              type, scalar.
!
!                              This is a diagnostic test case.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program procInterface001e
use m
    !procedure(integer), pointer :: pp1
     procedure(func1), pointer :: pp1
    !procedure(type(Base)), pointer :: pp2
     procedure(func2), pointer :: pp2
    class(Base(4)), pointer :: b1
    class(*), allocatable :: b2

    pp1 => func1
    pp2 => func2

    allocate(b1, SOURCE=Child(4)(3,4))
    print *, "func1", pp1(b1)

    allocate(b2, SOURCE=Base(4)(5))
    print *, "func2", pp2(b2)

    contains

    integer function func1(b)
        class(Base(4)), intent(in) :: b
        select type (b)
            type is (Base(4))
                func1 = b%i
            type is (Child(4))
                func1 = b%i+b%j
            class default
                error stop 1_4
        end select
    end function

    type(Base(4)) function func2(b)
        class(*), allocatable, intent(in) :: b
        select type (b)
            type is (Base(4))
                func2 = Base(4)(-b%i)
            type is (Child(4))
                func2 = Base(4)(b%i-b%j)
            class default
                error stop 2_4
        end select
    end function
end
