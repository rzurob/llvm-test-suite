! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_procptr/component2/functionReturn002a.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Specify
!                              proc-interface using declaration type
!                              specification. Poly, dummy arguments are
!                              allocatable, and return is allocatable.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
        !procedure(type(Base)), pointer, nopass :: pp1
         procedure(func2), pointer, nopass :: pp1
    end type

    contains

    function func1(b)
        class(Base(*,4)), allocatable, intent(in) :: b
        !procedure(type(Base)), pointer :: func1
         procedure(func2), pointer :: func1

        select type (b)
            type is (Base(*,4))
                func1 => func2
            type is (Child(*,4))
                func1 => func3
            class default
                error stop 1_4
        end select
    end function

    function func2(b)
        class(Base(*,4)), allocatable, intent(in) :: b
        type(Base(:,4)), allocatable :: func2
        select type (b)
            type is (Base(*,4))
                allocate(func2, SOURCE=Base(20,4)(b%i))
            type is (Child(*,4))
                allocate(func2, SOURCE=Base(20,4)(b%i+b%j))
            class default
                error stop 2_4
        end select
    end function

    function func3(b)
        class(Base(*,4)), allocatable, intent(in) :: b
        type(Base(:,4)), allocatable :: func3
        select type (b)
            type is (Base(*,4))
                allocate(func3, SOURCE=Base(20,4)(-b%i))
            type is (Child(*,4))
                allocate(func3, SOURCE=Base(20,4)(b%i-b%j))
            class default
                error stop 3_4
        end select
    end function
end module

program functionReturn002a
use m
    implicit type(Child(20,4)) (c)

    class(Base(20,4)), allocatable :: b1

    allocate(b1, SOURCE=Base(20,4)(5))
    c1%pp1 => func1(b1)
    print *, "func2", c1%pp1(b1)

    deallocate(b1)
    allocate(b1, SOURCE=Child(20,4)(12, 21, null()))
    c1%pp1 => func1(b1)
    print *, "func3", c1%pp1(b1)
end