! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_procptr/declaration2/interfaceName001e.f
! opt variations: -qnok -ql -qreuse=none

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
!                              This test case use explicit interface to
!                              declare the interface-name before calling
!                              external subroutine and function. The
!                              actual procedure associated has the same
!                              name as interface-name. The dummy
!                              arguments are pointer. Use abstract type.
!                              And interface-name is different from the
!                              name of the associated procedure.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    interface
        subroutine sub1(b)
        import AbstractParent
            class(AbstractParent(4)), pointer, intent(in) :: b
        end subroutine

        function func1(b)
        import AbstractParent
            class(AbstractParent(4)), pointer, intent(in) :: b
            class(AbstractParent(4)), pointer :: func1
        end function
    end interface
end module

program interfaceName001e
use m
    interface
        subroutine interfaceSub1(b)
        use m
            class(AbstractParent(4)), pointer, intent(in) :: b
        end subroutine

        function interfaceFunc1(b)
        use m
            class(AbstractParent(4)), pointer, intent(in) :: b
            class(AbstractParent(4)), pointer :: interfaceFunc1
        end function
    end interface

    procedure(interfaceSub1), pointer :: pp1
    procedure(interfaceFunc1), pointer :: pp2
    class(AbstractParent(4)), pointer :: b1

    pp1 => sub1
    pp2 => func1

    allocate(b1, SOURCE=Base(4)(10))
    call pp1(b1)
    select type (b=>pp2(b1))
        type is (Base(4))
            print *, "func1 Base", b
        type is (Child(4))
            print *, "func1 Child", b
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(4)(20,22))
    call pp1(b1)
    select type (b=>pp2(b1))
        type is (Base(4))
            print *, "func1 Base", b
        type is (Child(4))
            print *, "func1 Child", b
        class default
            error stop 2_4
    end select
end

subroutine sub1(b)
use m, only : AbstractParent, Base, Child
    class(AbstractParent(4)), pointer, intent(in) :: b
    select type (b)
        type is (Base(4))
            print *, "sub1 Base", b
        type is (Child(4))
            print *, "sub1 Child", b
        class default
            error stop 3_4
    end select
end subroutine

function func1(b)
use m, only : AbstractParent, Base, Child
    class(AbstractParent(4)), pointer, intent(in) :: b
    class(AbstractParent(4)), pointer :: func1
    select type (b)
        type is (Base(4))
            allocate(func1, SOURCE=Base(4)(b%i*2))
        type is (Child(4))
            allocate(func1, SOURCE=Child(4)(b%j,b%i))
        class default
            error stop 4_4
    end select
end function
