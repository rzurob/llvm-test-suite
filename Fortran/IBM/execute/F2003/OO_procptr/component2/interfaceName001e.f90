!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Specify procedure interface using
!                              interface-name, which is an external
!                              procedure. Poly, scalar or array.
!
!                              This test case use explicit interface to
!                              declare the interface-name before calling
!                              external subroutine and function. The
!                              dummy arguments are pointer. Use abstract
!                              type. And interface-name is different
!                              from the name of associated procedure.
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
    end type

    type, extends(Base) :: Child
        integer j
        procedure(interfaceSub1), pointer, nopass :: pp1
        procedure(interfaceFunc1), pointer, nopass :: pp2
    end type
end module

program interfaceName001e
use m
    interface
        subroutine sub1(b)
        import AbstractParent
            class(AbstractParent), pointer, intent(in) :: b
        end subroutine

        function func1(b)
        import AbstractParent
            class(AbstractParent), pointer, intent(in) :: b
            class(AbstractParent), pointer :: func1
        end function
    end interface

    class(AbstractParent), pointer :: b1
    class(AbstractParent), pointer :: c1

    allocate(c1, SOURCE=Child(1,1,null(),null()))

    select type (c1)
        class is (Child)
            c1%pp1 => sub1
            c1%pp2 => func1

            allocate(b1, SOURCE=Base(10))
            call c1%pp1(b1)
            select type (b=>c1%pp2(b1))
                type is (Base)
                    print *, "func1 Base", b
                type is (Child)
                    print *, "func1 Child", b%Base, b%j 
                class default
                    error stop 1_4
            end select

            deallocate(b1)
            allocate(b1, SOURCE=Child(20,22,null(),null()))
            call c1%pp1(b1)
            select type (b=>c1%pp2(b1))
                type is (Base)
                    print *, "func1 Base", b
                type is (Child)
                    print *, "func1 Child", b%Base, b%j 
                class default
                    error stop 2_4
            end select
        class default
            error stop 3_4
    end select
end

subroutine sub1(b)
use m, only : AbstractParent, Base, Child
    class(AbstractParent), pointer, intent(in) :: b
    select type (b)
        type is (Base)
            print *, "sub1 Base", b
        type is (Child)
            print *, "sub1 Child", b%Base, b%j 
        class default
            error stop 3_4
    end select
end subroutine

function func1(b)
use m, only : AbstractParent, Base, Child
    class(AbstractParent), pointer, intent(in) :: b
    class(AbstractParent), pointer :: func1
    select type (b)
        type is (Base)
            allocate(func1, SOURCE=Base(b%i*2))
        type is (Child)
            allocate(func1, SOURCE=Child(b%j,b%i, null(), null()))
        class default
            error stop 4_4
    end select
end function
