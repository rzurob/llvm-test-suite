! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_procptr/component2/procInterface002b.f
! opt variations: -qnok -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Do not specify proc-interface. The
!                              associated function is a module function.
!                              Poly and unlimited poly. Intrinsic or
!                              derived type, scalar.
!
!                              Involves abstract type and implicit
!                              typing. Function returns derived type.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

module m2
use m1
    implicit type(Base(4)) (b,c)

    type Container(k2)    ! (4)
        integer, kind :: k2
        procedure(func1), pointer, nopass :: bpp1
        procedure(func2), pointer, nopass :: cpp2
    end type

    contains

    function func1(b)
        class(AbstractParent(4)), intent(in) :: b
        type(Base(4)), allocatable :: func1
        select type (b)
            type is (Base(4))
                allocate(func1, SOURCE=Base(4)(b%i*2))
            type is (Child(4))
                allocate(func1, SOURCE=Base(4)(b%i+b%j))
            class default
                error stop 1_4
        end select
    end function

    function func2(b)
        class(*), allocatable, intent(in) :: b
        type(Base(4)) :: func2
        select type (b)
            type is (Base(4))
                func2 = Base(4)(b%i/2)
            type is (Child(4))
                func2 = Base(4)(b%i-b%j)
            class default
                error stop 2_4
        end select
    end function
end module

program procInterface002b
use m2
    type(Base(4)) :: rv1
    type(Container(4)) :: c1
    class(AbstractParent(4)), pointer :: b1
    class(*), allocatable :: b2

    c1%bpp1 => func1
    c1%cpp2 => func2

    allocate(b1, SOURCE=Child(4)(4,5))
    rv1 = c1%bpp1(b1)
    print *, "Func1", rv1

    allocate(b2, SOURCE=Base(4)(6))
    rv1 = c1%cpp2(b2)
    print *, "Func2", rv1
end
