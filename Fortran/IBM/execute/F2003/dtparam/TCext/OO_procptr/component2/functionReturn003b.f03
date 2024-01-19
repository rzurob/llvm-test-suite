! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_procptr/component2/functionReturn003b.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Do not
!                              specify proc-interface. Associate the
!                              procedure pointer to a function, with
!                              array dummy arguments. Unlimited poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    implicit type(Base(20,4)) (p)

    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
        procedure(func2), pointer, nopass :: pp1 => null()
    end type

    contains

    function func1(b)
        class(*), pointer :: b(:,:)
        procedure(func2), pointer :: func1

        select type (b)
            type is (Base(*,4))
                func1 => func2
            type is (Child(*,4))
                func1 => func3
            class default
                error stop 5_4
        end select
    end function

    function func2(b)
        class(*), pointer :: b(:,:)
        type(Base(:,4)), allocatable :: func2

        select type (b)
            type is (Base(*,4))
                allocate(func2, SOURCE=Base(20,4)(sum(b%i)))
            class default
                error stop 6_4
        end select
    end function

    function func3(b)
        class(*), pointer :: b(:,:)
        type(Base(:,4)), allocatable :: func3

        select type (b)
            type is (Child(*,4))
                allocate(func3, SOURCE=Base(20,4)(sum(b%i)*sum(b%j)))
            class default
                error stop 7_4
        end select
    end function
end module

program functionReturn003b
use m
    type(Child(:,4)), pointer :: c1
    class(*), pointer :: b1(:,:)

    allocate(Child(20,4)::c1)
    if(associated(c1%pp1)) error stop 1_4

    allocate(b1(2,3), SOURCE=reshape((/(Base(20,4)(i),i=1,6)/),(/2,3/)))
    c1%pp1 => func1(b1)
    if(.NOT. associated(c1%pp1)) error stop 2_4
    print *, "func2", c1%pp1(b1)

    c1%pp1 => null()
    if(associated(c1%pp1)) error stop 3_4

    deallocate(b1)
    allocate(b1(5,3), SOURCE=reshape((/(Child(20,4)(i,-i,null()),i=1,15)/), &
     (/5,3/)))
    c1%pp1 => func1(b1)
    if(.NOT. associated(c1%pp1)) error stop 4_4
    print *, "func3", c1%pp1(b1)
    deallocate(b1)
end
