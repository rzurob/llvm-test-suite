! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_procptr/component2/procInterface001a.f
! opt variations: -qnok -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The associated
!                              function is an external function.
!                              Poly and unlimited poly. Intrinsic or
!                              derived type, scalar.
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

    interface
        integer function ifunc1(b)
        import Base
            class(Base(4)), intent(in) :: b
        end function

        real function func2(b)
            class(*), pointer :: b
        end function
    end interface

    type Container(k2)    ! (4)
        integer, kind :: k2
        procedure(ifunc1), pointer, nopass :: pp1
        procedure(func2), pointer, nopass :: pp2
    end type
end module

program procInterface001a
use m
    class(*), pointer :: b1
    type(Container(4)) :: c1

    c1%pp1 => ifunc1
    c1%pp2 => func2

    print *, "ifunc1", c1%pp1(Child(4)(4, 5))
    allocate(b1, SOURCE=Base(4)(6))
    print *, "func2", int(c1%pp2(b1))
    deallocate(b1)
end

integer function ifunc1(b)
use m, only : Base, Child
    class(Base(4)), intent(in) :: b
    select type(b)
        type is (Base(4))
            ifunc1 = b%i * 2
        type is (Child(4))
            ifunc1 = b%i + b%j
        class default
            error stop 1_4
    end select
end function

real function func2(b)
use m, only : Base, Child
    class(*), pointer :: b
    select type(b)
        type is (Base(4))
            func2 = b%i / 2
        type is (Child(4))
            func2 = b%i - b%j
        class default
            error stop 2_4
    end select
end function
