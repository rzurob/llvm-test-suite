! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_procptr/declaration2/procInterface001c.f
! opt variations: -qnok -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The procedure pointer
!                              is a dummy argument. The associated
!                              function is a module function. Poly and
!                              unlimited poly. Intrinsic or derived
!                              type, scalar.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    contains

    type(Base(4)) function func1(b)
        class(AbstractParent(4)), allocatable, intent(in) :: b
        select type (b)
            type is (Base(4))
                func1 = Base(4)(b%i*2)
            type is (Child(4))
                func1 = Base(4)(b%i+b%j)
            class default
                error stop 1_4
        end select
    end function

    type(Base(4)) function func2(b, p)
        class(AbstractParent(4)), allocatable, intent(in) :: b
        !procedure(type(Base)), pointer, intent(in) :: p
         procedure(func1), pointer, intent(in) :: p
        func2 = p(b)
    end function
end module

program procInterface001c
use m
!    procedure(type(Base)), pointer :: pp1
!    procedure(type(Base)), pointer :: pp2

    procedure(func1), pointer :: pp1
    procedure(func2), pointer :: pp2


    class(AbstractParent(4)), allocatable :: b1

    pp1 => func1
    pp2 => func2

    allocate(b1, SOURCE=Base(4)(4))
    print *, pp2(b1, pp1)

    deallocate(b1)
    allocate(b1, SOURCE=Child(4)(5,6))
    print *, pp2(b1, pp1)
end
