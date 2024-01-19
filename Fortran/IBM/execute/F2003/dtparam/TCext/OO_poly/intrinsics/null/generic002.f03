! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/null/generic002.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 03/03/2005
! PRIMARY FUNCTIONS TESTED   : null
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    interface printMe
        subroutine printBase(b)
            import Base
            type(Base(4)), pointer :: b
        end subroutine

        subroutine printChild(c)
            import Child
            type(Child(4)), allocatable :: c
        end subroutine
    end interface printMe
end module

program generic002
use m
    type(Base(4)), pointer :: b1
    type(Child(4)), allocatable :: c1

    allocate(b1, SOURCE=Base(4)(10))
    allocate(c1, SOURCE=Child(4)(8, 9))

    call printMe(null(b1))
    call printMe(null(c1))
end

subroutine printBase(b)
use m, only : Base
    type(Base(4)), pointer :: b

    if(associated(b)) then
        print *, b
    else
        print *, "Base"
    end if
end subroutine

subroutine printChild(c)
use m, only : Child
    type(Child(4)), allocatable :: c

    if(allocated(c)) then
        print *, c
    else
        print *, "Child"
    end if
end subroutine
