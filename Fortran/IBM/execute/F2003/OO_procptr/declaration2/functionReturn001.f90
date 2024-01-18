!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Specify
!                              proc-interface using interface-name.
!                              Poly.
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

    contains

    subroutine sub1(b)
        class(Base), intent(in) :: b
        select type (b)
            type is (Base)
                print *, "sub1 Base", b
            type is (Child)
                print *, "sub1 Child", b
            class default
                error stop 1_4
        end select
    end subroutine

    subroutine sub2(b)
        class(Base), intent(in) :: b
        select type (b)
            type is (Base)
                print *, "sub2 Base", b
            type is (Child)
                print *, "sub2 Child", b
            class default
                error stop 2_4
        end select
    end subroutine

    function func1(b)
        class(Base), intent(in) :: b
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

program functionReturn001
use m
    class(Base), pointer :: b1
    procedure(sub1), pointer :: pp1

    allocate(b1, SOURCE=Base(10))
    pp1 => func1(b1)
    call pp1(b1)

    deallocate(b1)
    allocate(b1, SOURCE=Child(20, 30))
    pp1 => func1(b1)
    call pp1(b1)
end
