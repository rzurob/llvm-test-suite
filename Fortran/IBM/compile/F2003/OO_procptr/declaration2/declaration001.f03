!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : The procedure is a dummy argument and has
!                              the INTENT attribute. Then it must be
!                              declared with the POINTER attribute.
!                              Poly or unlimited poly.
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

    contains

    subroutine sub1(b, p)
        class(Base), pointer, intent(in) :: b
        procedure(integer), intent(in) :: p
        print *, "sub1"
    end subroutine

    integer function func1(b, p)
        class(*), allocatable, intent(in) :: b
        procedure(integer), intent(in) :: p
        func1 = 10
    end function
end module

program declaration001
end