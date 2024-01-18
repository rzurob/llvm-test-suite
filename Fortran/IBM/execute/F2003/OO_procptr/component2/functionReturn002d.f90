!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Specify
!                              proc-interface using declaration type
!                              specification. Poly, dummy arguments are
!                              allocatable and are arrays, and return
!                              is pointer.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    type, extends(Child) :: Container
        !procedure(type(Base)), pointer, nopass :: pp1
         procedure(func2), pointer, nopass :: pp1
    end type

    contains

    function func1(b)
        class(Base), allocatable, intent(in) :: b(:,:)
        !procedure(type(Base)), pointer :: func1
         procedure(func2), pointer :: func1

        select type (b)
            type is (Base)
                func1 => func2
            type is (Child)
                func1 => func3
            class default
                error stop 1_4
        end select
    end function

    function func2(b)
        class(Base), allocatable, intent(in) :: b(:,:)
        type(Base), pointer :: func2
        select type (b)
            type is (Base)
                allocate(func2, SOURCE=Base(size(b)))
            class default
                error stop 2_4
        end select
    end function

    function func3(b)
        class(Base), allocatable, intent(in) :: b(:,:)
        type(Base), pointer :: func3
        select type (b)
            type is (Child)
                allocate(func3, SOURCE=Base(size(b)*2))
            class default
                error stop 3_4
        end select
    end function
end module

program functionReturn002d
use m
    class(Base), allocatable :: b1(:,:)
    class(Container), pointer :: c1

    allocate(Container::c1)

    allocate(b1(4,3), SOURCE=reshape((/(Base(i),i=1,12)/),(/4,3/)))
    c1%pp1 => func1(b1)
    print *, "func2", c1%pp1(b1)

    deallocate(b1)
    allocate(b1(5,2), SOURCE=reshape((/(Child(i,i+2),i=1,10)/),(/5,2/)))
    c1%pp1 => func1(b1)
    print *, "func3", c1%pp1(b1)
end
