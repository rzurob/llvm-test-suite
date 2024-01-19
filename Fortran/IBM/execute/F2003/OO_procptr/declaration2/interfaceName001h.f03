!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either an external subroutine or
!                              an external function. Poly. Intrinsic
!                              or derived type, scalar or array.
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

module m
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program interfaceName001h
use m
    interface
        subroutine sub1(b)
        use m
            class(AbstractParent), intent(in) :: b
        end subroutine

        subroutine sub2(b)
        use m
            class(AbstractParent), intent(in) :: b
        end subroutine

        function func1(b)
        use m
            class(AbstractParent) :: b
            class(AbstractParent), pointer :: func1
        end function

        function func2(b)
        use m
            class(AbstractParent) :: b
            class(AbstractParent), pointer :: func2
        end function
    end interface

    procedure(sub1), pointer :: pp1 => null()
    procedure(func1), pointer :: pp2 => null()
    class(AbstractParent), pointer :: b1

    if(associated(pp1)) error stop 1_4
    if(associated(pp2)) error stop 2_4

    pp1 => sub1
    pp2 => func1

    if(.NOT. associated(pp1)) error stop 3_4
    if(.NOT. associated(pp2)) error stop 4_4

    allocate(b1, SOURCE=Base(10))
    call pp1(b1)
    select type (b=>pp2(b1))
        type is (Base)
            print *, "func1 Base", b
        class default
            error stop 5_4
    end select

    nullify(pp1)
    pp2 => null()

    if(associated(pp1)) error stop 6_4
    if(associated(pp2)) error stop 7_4

    pp1 => sub2
    pp2 => func2

    if(.NOT. associated(pp1)) error stop 8_4
    if(.NOT. associated(pp2)) error stop 9_4

    deallocate(b1)
    allocate(b1, SOURCE=Child(20,22))
    call pp1(b1)
    select type (b=>pp2(b1))
        type is (Child)
            print *, "func2 Child", b
        class default
            error stop 10_4
    end select
end

subroutine sub1(b)
use m
    class(AbstractParent), intent(in) :: b
    select type (b)
        type is (Base)
            print *, "sub1 Base", b
        class default
            error stop 11_4
    end select
end subroutine

function func1(b)
use m
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
use m
    class(AbstractParent), intent(in) :: b
    select type (b)
        type is (Child)
            print *, "sub2 Child", b
        class default
            error stop 13_4
    end select
end subroutine

function func2(b)
use m
    class(AbstractParent) :: b
    class(AbstractParent), pointer :: func2
    select type (b)
        type is (Child)
            allocate(func2, SOURCE=Child(b%j,b%i))
        class default
            error stop 14_4
    end select
end function
