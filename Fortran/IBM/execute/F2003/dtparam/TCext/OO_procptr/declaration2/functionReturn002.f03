! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_procptr/declaration2/functionReturn002.f
! opt variations: -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Specify
!                              proc-interface using declaration type
!                              specification. Poly.
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

    contains

    function func1(b)
        class(Base(4)), intent(in) :: b
        !procedure(type(Base)), pointer :: func1
         procedure(func2), pointer :: func1

        select type (b)
            type is (Base(4))
                func1 => func2
            type is (Child(4))
                func1 => func3
            class default
                error stop 1_4
        end select
    end function

    function func2(b)
        class(Base(4)), intent(in) :: b
        type(Base(4)), allocatable :: func2
        select type (b)
            type is (Base(4))
                allocate(func2, SOURCE=Base(4)(b%i))
            type is (Child(4))
                allocate(func2, SOURCE=Base(4)(b%i+b%j))
            class default
                error stop 2_4
        end select
    end function

    function func3(b)
        class(Base(4)), intent(in) :: b
        type(Base(4)), allocatable :: func3
        select type (b)
            type is (Base(4))
                allocate(func3, SOURCE=Base(4)(-b%i))
            type is (Child(4))
                allocate(func3, SOURCE=Base(4)(b%i-b%j))
            class default
                error stop 3_4
        end select
    end function
end module

program functionReturn002
use m
    class(Base(4)), allocatable :: b1
    !procedure(type(Base)), pointer :: pp1
     procedure(func2), pointer :: pp1

    allocate(b1, SOURCE=Base(4)(5))
    pp1 => func1(b1)
    print *, "func2", pp1(b1)

    deallocate(b1)
    allocate(b1, SOURCE=Child(4)(12, 21))
    pp1 => func1(b1)
    print *, "func3", pp1(b1)
end
