!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Specify procedure interface using
!                              interface-name, which is a module
!                              procedure. Poly, scalar or array.
!
!                              This test case use explicit interface
!                              implied by use association to declare the
!                              interface-name before calling module
!                              subroutine and function. The actual
!                              procedure associated has the same name as
!                              interface-name. The dummy arguments of
!                              the associated procedure are arrays. This
!                              test case involves sequence association
!                              where the size and shape of the actual
!                              argument do not match those of the
!                              dummy argument.
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
        class(AbstractParent), intent(in) :: b(2,4)
        select type (b)
            type is (Base)
                print *, "sub1 Base", b
            type is (Child)
                print *, "sub1 Child", b
            class default
                error stop 1_4
        end select
    end subroutine

    integer function func1(b)
        class(AbstractParent) :: b(3:*)
        select type (b)
            type is (Base)
                print *, "func1", b(:8)
                func1 = size(b(:8))
            class default
                error stop 2_4
        end select
    end function

    type(Base) function func2(b)
        class(*) :: b(5,3)
        select type (b)
            type is (Child)
                print *, "func2", b
                func2 = Base(size(b))
            class default
                error stop 3_4
        end select
    end function
end module

module m2
use m1
    type Container
        procedure(sub1), pointer, nopass :: pp1
        procedure(func1), pointer, nopass :: pp2
        procedure(func2), pointer, nopass :: pp3
    end type
end module

program interfaceName002i
use m2
    integer rv
    class(AbstractParent), pointer :: b1(:,:)
    class(*), allocatable :: b2(:,:,:)
    class(Container), pointer :: c1

    allocate(c1, SOURCE=Container(null(),null(),null()))
    c1%pp1 => sub1
    c1%pp2 => func1
    c1%pp3 => func2

    call c1%pp1(reshape((/(Base(i),i=1,18)/),(/3,3,3/), &
     (/Base(-1),Base(-2)/),(/2,3,1/)))

    allocate(b1(3,2), SOURCE=reshape((/(Base(i),i=1,6)/),(/3,2/), &
     (/Base(-1)/),(/2,1/)))
    rv = c1%pp2(b1)
    print *, "Func1", rv

    allocate(b2(4,3,2), SOURCE=reshape((/(Child(i,i+2),i=11,25)/), &
     (/4,3,2/), (/Child(-1,-3),Child(-2,-4)/), (/2,3,1/)))
    associate(name1=>c1%pp3(b2))
        print *, "Func2", name1
    end associate
end