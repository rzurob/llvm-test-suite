!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
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
    end type
end module

program procInterface002a
use m
    interface
        subroutine sub1(b)
        use m
            class(AbstractParent), intent(in) :: b
        end subroutine

        real function func2(b)
        use m
            class(*), pointer :: b
        end function
    end interface

    !procedure(), pointer :: pp1
     procedure(sub1), pointer :: pp1
    !procedure(), pointer :: pp2
     procedure(func2), pointer :: pp2
    class(*), pointer :: b1

    pp1 => sub1
    pp2 => func2

    call pp1(Child(4, 5))

    allocate(b1, SOURCE=Base(6))
    print *, "func2", int(pp2(b1))
end

subroutine sub1(b)
use m
    class(AbstractParent), intent(in) :: b
    select type(b)
        type is (Base)
            print *, "sub1 Base", b
        type is (Child)
            print *, "sub1 Child", b
        class default
            error stop 1_4
    end select
end subroutine

real function func2(b)
use m
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
