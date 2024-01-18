! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=none -qdeferredlp /tstdev/OO_poly/intrinsics/null/generic003.f
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
!                              procedure. Poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)      j
    end type

    interface printMe
        subroutine printBase(b)
            import Base
            class(Base(4)), pointer :: b
        end subroutine

        subroutine printChild(c)
            import Child
            class(Child(4,4)), allocatable :: c(:)
        end subroutine
    end interface printMe
end module

program generic003
use m
    class(Base(4)), pointer :: b1
    class(Child(4,4)), allocatable :: c1(:)

    allocate(b1, SOURCE=Child(4,4)(6,7))
    allocate(c1(3), SOURCE=Child(4,4)(8, 9))

    call printMe(b1)
    call printMe(null(b1))

    call printMe(c1)
    call printMe(null(c1))
end

subroutine printBase(b)
use m, only : Base, Child
    class(Base(4)), pointer :: b

    if(associated(b)) then
        select type(b)
            type is (Base(4))
                print *, "Base-Base", b
            type is (Child(4,4))
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
    class(Child(4,4)), allocatable :: c(:)

    if(allocated(c)) then
        select type(c)
            type is (Child(4,4))
                print *, "Child-Child", c
            class default
                error stop 2_4
        end select
    else
        print *, "Child"
    end if
end subroutine
