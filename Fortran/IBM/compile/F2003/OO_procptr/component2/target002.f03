!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Associate a proc-target with a data
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
    type Base
        integer i
    end type

    interface
        subroutine sub1(b, c)
        !use m
        import Base
            class(Base), intent(in) :: b
            class(*), intent(in) :: c
        end subroutine

        type(Base) function func1(b, c)
        !use m
        import Base
            class(Base), pointer :: b
            class(*), allocatable :: c
        end function
    end interface

    type, extends(Base) :: Child
        integer j
        procedure(sub1), pointer, nopass :: pp1
        procedure(func1), pointer, nopass :: pp2
    end type
end module

program target002
use m
    type(Base), pointer :: b1
    type(Child) :: c1

    c1%pp1 => sub1
    c1%pp2 => func1

    b1 => c1%pp1
    b1 => c1%pp2
end

subroutine sub1(b, c)
use m, only : Base
    class(Base), intent(in) :: b
    class(*), intent(in) :: c
    print *, "sub1", b%i
end subroutine

type(Base) function func1(b, c)
use m, only : Base
    class(Base), pointer :: b
    class(*), allocatable :: c
    func1 = Base(b%i)
end function
