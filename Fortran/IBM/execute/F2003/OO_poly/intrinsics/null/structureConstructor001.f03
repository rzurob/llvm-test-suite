!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 02/27/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : null is used in a structure constructor.
!                              Non-poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer :: i = 8
    end type

    type, extends(Base) :: Child
        integer :: j = 9
    end type

    type Container
        type(Base), pointer :: b => null()
        type(Child), pointer :: c
    end type
end module

program structureConstructor001
use m
    type(Container) :: a1

    ! a1%b is disassociated by default
    if(associated(a1%b)) error stop 1_4

    a1 = Container(null(), null())

    ! a1%c is disassociated by structure constructor
    if(associated(a1%c)) error stop 2_4
end