!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Specify procedure interface using
!                              interface-name, which is a module
!                              procedure. Poly, scalar or array.
!
!                              This test case test the association
!                              status of procedure pointer with the help
!                              of ASSOCIATED(), NULL(), and NULLIFY().
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    subroutine sub1(b)
        class(AbstractParent), intent(in) :: b
        select type (b)
            type is (Base)
                print *, "sub1 Base", b
            class default
                error stop 11_4
        end select
    end subroutine

    function func1(b)
        class(AbstractParent) :: b
        class(AbstractParent), pointer :: func1
        select type (b)
            type is (Base)
                allocate(func1, SOURCE=Base(b%i*2))
            class default
                error stop 12_4
        end select
    end function

    subroutine sub2(b)
        class(AbstractParent), intent(in) :: b
        select type (b)
            type is (Child)
                print *, "sub2 Child", b
            class default
                error stop 13_4
        end select
    end subroutine

    function func2(b)
        class(AbstractParent) :: b
        class(AbstractParent), pointer :: func2
        select type (b)
            type is (Child)
                allocate(func2, SOURCE=Child(b%j,b%i))
            class default
                error stop 14_4
        end select
    end function
end module

module m2
use m1
    type Container
        procedure(sub1), pointer, nopass :: pp1 => null()
        procedure(func1), pointer, nopass :: pp2 => null()
    end type
end module

program interfaceName002h
use m2
    implicit type(Container) (c)

    class(AbstractParent), pointer :: b1

    if(associated(c1%pp1)) error stop 1_4
    if(associated(c1%pp2)) error stop 2_4

    c1%pp1 => sub1
    c1%pp2 => func1

    if(.NOT. associated(c1%pp1)) error stop 3_4
    if(.NOT. associated(c1%pp2)) error stop 4_4

    allocate(b1, SOURCE=Base(10))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base)
            print *, "func1 Base", b
        class default
            error stop 5_4
    end select

    nullify(c1%pp1)
    c1%pp2 => null()

    if(associated(c1%pp1)) error stop 6_4
    if(associated(c1%pp2)) error stop 7_4

    c1%pp1 => sub2
    c1%pp2 => func2

    if(.NOT. associated(c1%pp1)) error stop 8_4
    if(.NOT. associated(c1%pp2)) error stop 9_4

    deallocate(b1)
    allocate(b1, SOURCE=Child(20,22))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Child)
            print *, "func2 Child", b
        class default
            error stop 10_4
    end select
end
