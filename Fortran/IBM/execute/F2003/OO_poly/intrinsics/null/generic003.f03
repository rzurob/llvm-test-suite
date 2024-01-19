!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 03/03/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : The context of the reference to null
!                              is an actual argument to a generic
!                              procedure. Poly.
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

    interface printMe
        subroutine printBase(b)
            import Base
            class(Base), pointer :: b
        end subroutine

        subroutine printChild(c)
            import Child
            class(Child), allocatable :: c(:)
        end subroutine
    end interface printMe
end module

program generic003
use m
    class(Base), pointer :: b1
    class(Child), allocatable :: c1(:)

    allocate(b1, SOURCE=Child(6,7))
    allocate(c1(3), SOURCE=Child(8, 9))

    call printMe(b1)
    call printMe(null(b1))

    call printMe(c1)
    call printMe(null(c1))
end

subroutine printBase(b)
use m, only : Base, Child
    class(Base), pointer :: b

    if(associated(b)) then
        select type(b)
            type is (Base)
                print *, "Base-Base", b
            type is (Child)
                print *, "Base-Child", b
            class default
                error stop 1_4
        end select
    else
        print *, "Base"
    end if
end subroutine

subroutine printChild(c)
use m, only : Base, Child
    class(Child), allocatable :: c(:)

    if(allocated(c)) then
        select type(c)
            type is (Child)
                print *, "Child-Child", c
            class default
                error stop 2_4
        end select
    else
        print *, "Child"
    end if
end subroutine
