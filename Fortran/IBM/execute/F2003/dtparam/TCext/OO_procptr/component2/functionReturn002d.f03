! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_procptr/component2/functionReturn002d.f
! opt variations: -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    type, extends(Child) :: Container    ! (4)
        !procedure(type(Base)), pointer, nopass :: pp1
         procedure(func2), pointer, nopass :: pp1
    end type

    contains

    function func1(b)
        class(Base(4)), allocatable, intent(in) :: b(:,:)
        !procedure(type(Base)), pointer :: func1
         procedure(func2), pointer :: func1

        select type (b)
            type is (Base(4))
                func1 => func2
            type is (Child(4))
                func1 => func3
            class default
                error stop 1_4
        end select
    end function

    function func2(b)
        class(Base(4)), allocatable, intent(in) :: b(:,:)
        type(Base(4)), pointer :: func2
        select type (b)
            type is (Base(4))
                allocate(func2, SOURCE=Base(4)(size(b)))
            class default
                error stop 2_4
        end select
    end function

    function func3(b)
        class(Base(4)), allocatable, intent(in) :: b(:,:)
        type(Base(4)), pointer :: func3
        select type (b)
            type is (Child(4))
                allocate(func3, SOURCE=Base(4)(size(b)*2))
            class default
                error stop 3_4
        end select
    end function
end module

program functionReturn002d
use m
    class(Base(4)), allocatable :: b1(:,:)
    class(Container(4)), pointer :: c1

    allocate(Container(4)::c1)

    allocate(b1(4,3), SOURCE=reshape((/(Base(4)(i),i=1,12)/),(/4,3/)))
    c1%pp1 => func1(b1)
    print *, "func2", c1%pp1(b1)

    deallocate(b1)
    allocate(b1(5,2), SOURCE=reshape((/(Child(4)(i,i+2),i=1,10)/),(/5,2/)))
    c1%pp1 => func1(b1)
    print *, "func3", c1%pp1(b1)
end
