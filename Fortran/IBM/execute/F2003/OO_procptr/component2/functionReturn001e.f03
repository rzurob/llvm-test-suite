!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Specify
!                              proc-interface using interface-name.
!                              Poly. Dummy arguments are array.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

module m2
use m1
    contains

    subroutine sub1(b)
        class(Base), pointer, intent(in) :: b(:,:)
        select type (b)
            type is (Base)
                print *, "sub1 Base", b
                print *, "sub1 Base", shape(b)
            type is (Child)
                print *, "sub1 Child", b
                print *, "sub1 Child", shape(b)
            class default
                error stop 1_4
        end select
    end subroutine

    subroutine sub2(b)
        class(Base), pointer, intent(in) :: b(:,:)
        select type (b)
            type is (Base)
                print *, "sub2 Base", b
                print *, "sub2 Base", shape(b)
            type is (Child)
                print *, "sub2 Child", b
                print *, "sub2 Child", shape(b)
            class default
                error stop 2_4
        end select
    end subroutine

    function func1(b)
        class(Base), pointer, intent(in) :: b(:,:)
        procedure(sub1), pointer :: func1

        select type (b)
            type is (Base)
                func1 => sub1
            type is (Child)
                func1 => sub2
            class default
                error stop 3_4
        end select
    end function
end module

module m3
use m2, only : sub1
    type Container
        procedure(sub1), pointer, nopass :: pp1
    end type
end module

program functionReturn001e
use m2
use m3
    implicit type(Container) (c)
    class(Base), pointer :: b1(:,:)

    allocate(b1(3,4), SOURCE=reshape((/(Base(i),i=1,12)/),(/3,4/)))
    c1%pp1 => func1(b1)
    call c1%pp1(b1)

    deallocate(b1)
    allocate(b1(5,3), SOURCE=reshape((/(Child(i,-i),i=1,15)/),(/5,3/)))
    c1%pp1 => func1(b1)
    call c1%pp1(b1)
end
