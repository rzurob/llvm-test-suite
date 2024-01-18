!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! PROGRAMMER                 : Yong Du
! DATE                       : 03/03/2005
! PRIMARY FUNCTIONS TESTED   : null
! DRIVER STANZA              : xlf90
! DESCRIPTION                : The context of the reference to null
!                              is an actual argument to a generic
!                              procedure. Non-poly.
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
            type(Base), pointer :: b
        end subroutine

        subroutine printChild(c)
            import Child
            type(Child), allocatable :: c
        end subroutine
    end interface printMe
end module

program generic002
use m
    type(Base), pointer :: b1
    type(Child), allocatable :: c1

    allocate(b1, SOURCE=Base(10))
    allocate(c1, SOURCE=Child(8, 9))

    call printMe(null(b1))
    call printMe(null(c1))
end

subroutine printBase(b)
use m, only : Base
    type(Base), pointer :: b

    if(associated(b)) then
        print *, b
    else
        print *, "Base"
    end if
end subroutine

subroutine printChild(c)
use m, only : Child
    type(Child), allocatable :: c

    if(allocated(c)) then
        print *, c
    else
        print *, "Child"
    end if
end subroutine
