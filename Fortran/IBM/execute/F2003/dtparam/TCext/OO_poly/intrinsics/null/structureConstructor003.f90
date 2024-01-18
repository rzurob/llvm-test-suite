! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/null/structureConstructor003.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 03/01/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : null is used in a structure constructor.
!                              Unlimited poly.
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

    type Container(k2,n1)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n1
        class(*), pointer :: b => null()
        class(*), pointer :: c
    end type
end module

program structureConstructor003
use m
    type(Container(4,20)) :: a1

    ! a1%b is disassociated by default
    if(associated(a1%b)) error stop 1_4

    a1 = Container(4,20)(null(), null())

    ! a1%c is disassociated by structure constructor
    if(associated(a1%c)) error stop 2_4
end
