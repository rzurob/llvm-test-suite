! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/null/structureConstructor002.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! PROGRAMMER                 : Yong Du
! DATE                       : 03/01/2005
! PRIMARY FUNCTIONS TESTED   : null
! DRIVER STANZA              : xlf90
! DESCRIPTION                : null is used in a structure constructor.
!                              Poly.
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
        integer(k1)   :: i = 8
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) :: j = 9
    end type

    type Container(k2)    ! (4)
        integer, kind             :: k2
        class(Base(k2)), pointer  :: b => null()
        class(Child(k2)), pointer :: c
    end type
end module

program structureConstructor002
use m
    type(Container(4)) :: a1

    ! a1%b is disassociated by default
    if(associated(a1%b)) error stop 1_4

    a1 = Container(4)(null(), null())

    ! a1%c is disassociated by structure constructor
    if(associated(a1%c)) error stop 2_4
end
