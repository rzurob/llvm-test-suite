!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either an external subroutine or
!                              an external function. Unlimited Poly.
!                              Intrinsic or derived type, scalar or
!                              array.
!
!                              This test case use explicit interface
!                              implied by use association to call
!                              external procedures. The actual procedure
!                              name has the same name as interface-name.
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

    interface
        subroutine sub1(b)
            class(*), intent(in) :: b
        end subroutine

        function func1(b)
        import Base
            class(*) :: b
            class(Base), allocatable :: func1(:)
        end function
    end interface
end module

program interfaceName001b
use m
    procedure(sub1), pointer :: pp1
    procedure(func1), pointer :: pp2
    class(*), pointer :: b1

    pp1 => sub1
    pp2 => func1

    allocate(b1, SOURCE=Base(10))
    call pp1(b1)
    select type (b=>pp2(b1))
        type is (Base)
            print *, "func1 Base", b
        type is (Child)
            print *, "func1 Child", b
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(20,22))
    call pp1(b1)
    select type (b=>pp2(b1))
        type is (Base)
            print *, "func1 Base", b
        type is (Child)
            print *, "func1 Child", b
        class default
            error stop 2_4
    end select
end

subroutine sub1(b)
use m, only : Base, Child
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
use m, only : Base, Child
    class(*) :: b
    class(Base), allocatable :: func1(:)
    select type (b)
        type is (Base)
            allocate(func1(5), SOURCE=(/(Base(j+b%i),j=1,5)/))
        type is (Child)
            allocate(func1(8), SOURCE=(/(Child(j+b%i,j+b%j),j=1,8)/))
        class default
            error stop 4_4
    end select
end function
