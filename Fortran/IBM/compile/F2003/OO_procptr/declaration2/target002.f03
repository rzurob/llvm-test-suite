!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
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

    type, extends(Base) :: Child
        integer j
    end type
end module

program target002
use m
    interface
        subroutine sub1(b, c)
        use m
            class(Base), intent(in) :: b
            class(*), intent(in) :: c
        end subroutine

        type(Base) function func1(b, c)
        use m
            class(Base), pointer :: b
            class(*), allocatable :: c
        end function
    end interface

    procedure(sub1), pointer :: pp1
    procedure(func1), pointer :: pp2
    type(Base), pointer :: b1

    pp1 => sub1
    pp2 => func1

    b1 => pp1
    b1 => pp2
end

subroutine sub1(b, c)
use m
    class(Base), intent(in) :: b
    class(*), intent(in) :: c
    print *, "sub1", b%i
end subroutine

type(Base) function func1(b, c)
use m
    class(Base), pointer :: b
    class(*), allocatable :: c
    func1 = Base(b%i)
end function