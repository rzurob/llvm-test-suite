! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_procptr/component2/target001.f
! opt variations: -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Associate a data-target with a procedure
!                              pointer. Poly and unlimited poly.
!
!                              This test case is diagnostic.
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

    interface
        subroutine sub1(b, c)
        import Base
            class(Base(4)), intent(in) :: b
            class(*), intent(in) :: c
        end subroutine

        type(Base(4)) function func1(b, c)
        import Base
            class(Base(4)), pointer :: b
            class(*), allocatable :: c
        end function
    end interface

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
        procedure(sub1), pointer, nopass :: pp1
        procedure(func1), pointer, nopass :: pp2
    end type
end module

program target001
use m
    type(Child(4)) :: c1
    type(Base(4)), target :: b1

    b1 = Base(4)(10)

    c1%pp1 => b1
    c1%pp2 => b1
end

subroutine sub1(b, c)
use m, only : Base
    class(Base(4)), intent(in) :: b
    class(*), intent(in) :: c
    print *, "sub1", b%i
end subroutine

type(Base(4)) function func1(b, c)
use m, only : Base
    class(Base(4)), pointer :: b
    class(*), allocatable :: c
    func1 = Base(4)(b%i)
end function