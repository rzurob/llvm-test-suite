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

    interface
        subroutine interfaceSub1(b)
        import AbstractParent
            class(AbstractParent), pointer, intent(in) :: b
        end subroutine

        function interfaceFunc1(b)
        import AbstractParent
            class(AbstractParent), pointer, intent(in) :: b
            class(AbstractParent), pointer :: interfaceFunc1
        end function
    end interface

    type, extends(AbstractParent) :: Base
        integer i
        procedure(interfaceSub1), pointer, nopass :: pp1
        procedure(interfaceFunc1), pointer, nopass :: pp2
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    subroutine sub1(b)
        class(AbstractParent), pointer, intent(in) :: b
        select type (b)
            type is (Base)
                print *, "sub1 Base", b%i
            type is (Child)
                print *, "sub1 Child", b%i, b%j
            class default
                error stop 3_4
        end select
    end subroutine

    function func1(b)
        class(AbstractParent), pointer, intent(in) :: b
        class(AbstractParent), pointer :: func1
        select type (b)
            type is (Base)
                allocate(func1, SOURCE=Base(b%i*2,null(),null()))
            type is (Child)
                allocate(func1, SOURCE=Child(b%j,null(),null(),b%i))
            class default
                error stop 4_4
        end select
    end function
end module

program interfaceName002e
use m
    class(AbstractParent), pointer :: b1
    class(Base), allocatable :: b2

    allocate(Base::b2)
    b2%pp1 => sub1
    b2%pp2 => func1

    allocate(b1, SOURCE=Base(10,null(),null()))
    call b2%pp1(b1)
    select type (b=>b2%pp2(b1))
        type is (Base)
            print *, "func1 Base", b%i
        type is (Child)
            print *, "func1 Child", b%i, b%j
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(20,null(),null(),22))
    call b2%pp1(b1)
    select type (b=>b2%pp2(b1))
        type is (Base)
            print *, "func1 Base", b%i
        type is (Child)
            print *, "func1 Child", b%i, b%j
        class default
            error stop 2_4
    end select
end
