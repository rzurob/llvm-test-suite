!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Do not specify proc-interface. The
!                              associated function is an external
!                              function. Poly and unlimited poly.
!                              Intrinsic or derived type, scalar.
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
        procedure(sub1), pointer, nopass :: pp1
        procedure(func2), pointer, nopass :: pp2
    end type

    interface
        subroutine sub1(b)
            import
            class(AbstractParent), intent(in) :: b
        end subroutine

        real function func2(b)
            import
            class(*), pointer :: b
        end function
    end interface

end module

program procInterface002a
use m
    class(*), pointer :: b1
    type(Child) :: c1

    c1%pp1 => sub1
    c1%pp2 => func2

    call c1%pp1(Child(4, 5, null(), null()))

    allocate(b1, SOURCE=Base(6))
    print *, "func2", int(c1%pp2(b1))
end

subroutine sub1(b)
use m, only: AbstractParent, Base, Child
    class(AbstractParent), intent(in) :: b
    select type(b)
        type is (Base)
            print *, "sub1 Base", b
        type is (Child)
            print *, "sub1 Child", b%i, b%j
        class default
            error stop 1_4
    end select
end subroutine

real function func2(b)
use m, only: AbstractParent, Base, Child
    class(*), pointer :: b
    select type(b)
        type is (Base)
            func2 = b%i / 2
        type is (Child)
            func2 = b%i - b%j
        class default
            error stop 2_4
    end select
end function
