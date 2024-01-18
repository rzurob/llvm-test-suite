!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! PROGRAMMER                 : Yong Du
! DATE                       : 03/01/2005
! PRIMARY FUNCTIONS TESTED   : null
! DRIVER STANZA              : xlf90
! DESCRIPTION                : null is used in a DATA statement.
!                              Poly.
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
        class(Base), pointer :: b
        class(Child), pointer :: c
    end type
end module

program data002
use m
    type(Container) :: c1
    DATA c1 / Container( null(), c=null() ) /

    class(Base), pointer :: b1
    DATA b1 / null() /

    if(associated(c1%b)) error stop 1_4
    if(associated(c1%c)) error stop 2_4
    if(associated(b1)) error stop 3_4
end
