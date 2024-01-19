! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_procptr/declaration2/functionReturn001f.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Function
!                              return is a function, which returns
!                              a poly data entity. Specify
!                              proc-interface using interface-name.
!                              Poly. Dummy arguments are array.
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
    end type

    contains

    function func2(b)
        class(Base(*,4)), allocatable, intent(in) :: b(:,:)
        class(Base(:,4)), pointer :: func2
        select type (b)
            type is (Base(*,4))
                allocate(func2, SOURCE=Base(20,4)(size(b)))
            class default
                error stop 3_4
        end select
    end function

    function func3(b)
        class(Base(*,4)), allocatable, intent(in) :: b(:,:)
        class(Base(:,4)), pointer :: func3
        select type (b)
            type is (Child(*,4))
                allocate(func3, SOURCE=Child(20,4)(size(b),-size(b)))
            class default
                error stop 4_4
        end select
    end function

    function func1(b)
        class(Base(*,4)), allocatable, intent(in) :: b(:,:)
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
end module

program functionReturn001f
use m
    class(Base(20,4)), allocatable :: b1(:,:)
    procedure(func2), pointer :: pp1

    allocate(b1(3,4), SOURCE=reshape((/(Base(20,4)(i),i=1,12)/),(/3,4/)))
    pp1 => func1(b1)
    select type (name1=>pp1(b1))
        type is (Base(*,4))
            print *, name1
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1(5,3), SOURCE=reshape((/(Child(20,4)(i,-i),i=1,15)/),(/5,3/)))
    pp1 => func1(b1)
    select type (name1=>pp1(b1))
        type is (Child(*,4))
            print *, name1
        class default
            error stop 2_4
    end select
end
