!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Specify procedure interface using
!                              interface-name, which is a module
!                              procedure. Unlimited poly, scalar or
!                              array.
!
!                              This test case use explicit interface
!                              implied by use association to call
!                              module procedures. The actual procedure
!                              name has the same name as interface-name.
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
        class(*), intent(in) :: b
        select type (b)
            type is (Base)
                print *, "sub1 Base", b
            type is (Child)
                print *, "sub1 Child", b
            class default
                error stop 3_4
        end select
    end subroutine

    function func1(b)
        class(*) :: b
        class(Base), allocatable :: func1(:)
        select type (b)
            type is (Base)
                allocate(func1(5), SOURCE=(/(Base(j+b%i),j=1,5)/))
            type is (Child)
                allocate(func1(8),SOURCE=(/(Child(j+b%i,j+b%j),j=1,8)/))
            class default
                error stop 4_4
        end select
    end function
end module

module m2
use m1
    type Container
        procedure(sub1), pointer, nopass :: pp1
        procedure(func1), pointer, nopass :: pp2
    end type
end module

program interfaceName002b
use m2
    implicit type(Container) (c)

    class(*), pointer :: b1

    c1%pp1 => sub1
    c1%pp2 => func1

    allocate(b1, SOURCE=Base(10))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base)
            print *, "func1 Base", b
        type is (Child)
            print *, "func1 Child", b
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(20,22))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base)
            print *, "func1 Base", b
        type is (Child)
            print *, "func1 Child", b
        class default
            error stop 2_4
    end select
end
