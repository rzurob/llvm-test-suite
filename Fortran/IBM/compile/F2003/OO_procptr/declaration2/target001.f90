!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
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
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program target001
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

    type(Base), target :: b1
    b1 = Base(10)

    pp1 => b1
    pp2 => b1
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
