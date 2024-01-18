! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/null/structureConstructor001.f
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: i = 8
    end type

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)   :: j = 9
    end type

    type Container(n1)    ! (20)
        integer, len              :: n1
        type(Base(4)), pointer    :: b => null()
        type(Child(4,4)), pointer :: c
    end type
end module

program structureConstructor001
use m
    type(Container(20)) :: a1

    ! a1%b is disassociated by default
    if(associated(a1%b)) error stop 1_4

    a1 = Container(20)(null(), null())

    ! a1%c is disassociated by structure constructor
    if(associated(a1%c)) error stop 2_4
end
