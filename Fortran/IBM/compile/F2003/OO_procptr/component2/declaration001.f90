!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Assiign the procedure pointer to a
!                              non-intrinsic elemental procedure.
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

    type(Base) elemental function func1(b)
        class(Base), intent(in) :: b
        func1 = Base(b%i)
    end function
end module

program declaration001
use m
    type Container
        procedure(type(Base)), pointer, nopass :: p
    end type

    type(Container) :: c1
    c1%p => func1
end
