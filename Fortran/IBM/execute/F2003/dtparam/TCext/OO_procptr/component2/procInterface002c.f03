! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_procptr/component2/procInterface002c.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Do not specify proc-interface. The
!                              procedure pointer is a dummy argument.
!                              The associated function is a module
!                              function. Poly and unlimited poly.
!                              Intrinsic or derived type, scalar or
!                              array.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type

    contains

    type(Base(4,20)) function func1(b)
        class(AbstractParent(4,*)), allocatable, intent(in) :: b
        select type (b)
            type is (Base(4,*))
                func1 = Base(4,20)(b%i*2)
            type is (Child(4,*))
                func1 = Base(4,20)(b%i+b%j)
            class default
                error stop 1_4
        end select
    end function

    type(Base(4,20)) function func2(b, p)
        implicit type(Base(4,20)) (p)
        class(AbstractParent(4,*)), allocatable, intent(in) :: b
        procedure(func1), pointer, intent(in) :: p
        func2 = p(b)
    end function
end module

module m2
use m1
    implicit type(Base(4,20)) (p)

    type Container(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        procedure(func1), pointer, nopass :: pp1
        procedure(func2), pointer, nopass :: pp2
    end type
end module

program procInterface002c
use m2
    implicit type(Container(4,20)) (c)

    class(AbstractParent(4,20)), allocatable :: b1

    c1%pp1 => func1
    c1%pp2 => func2

    allocate(b1, SOURCE=Base(4,20)(4))
    print *, c1%pp2(b1, c1%pp1)

    deallocate(b1)
    allocate(b1, SOURCE=Child(4,20)(5,6))
    print *, c1%pp2(b1, c1%pp1)
end