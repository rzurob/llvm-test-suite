!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either a module subroutine or
!                              a module function. Poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case use explicit interface
!                              implied by use association to declare the
!                              interface-name before calling module
!                              subroutine and function. The actual
!                              procedure associated has the same name as
!                              interface-name. The dummy arguments are
!                              pointer. Use abstract type. And
!                              interface-name is different from the name
!                              of the associated procedure.
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

    contains

    subroutine sub1(b)
        class(AbstractParent), pointer, intent(in) :: b
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
        class(AbstractParent), pointer, intent(in) :: b
        class(AbstractParent), pointer :: func1
        select type (b)
            type is (Base)
                allocate(func1, SOURCE=Base(b%i*2))
            type is (Child)
                allocate(func1, SOURCE=Child(b%j,b%i))
            class default
                error stop 4_4
        end select
    end function
end module

program interfaceName002e
use m
    interface
        subroutine interfaceSub1(b)
        use m
            class(AbstractParent), pointer, intent(in) :: b
        end subroutine

        function interfaceFunc1(b)
        use m
            class(AbstractParent), pointer, intent(in) :: b
            class(AbstractParent), pointer :: interfaceFunc1
        end function
    end interface

    procedure(interfaceSub1), pointer :: pp1
    procedure(interfaceFunc1), pointer :: pp2
    class(AbstractParent), pointer :: b1

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
