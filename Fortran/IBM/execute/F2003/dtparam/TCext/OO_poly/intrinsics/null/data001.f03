! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/null/data001.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 03/01/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : null is used in a DATA statement.
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

    type, extends(Base) :: Child    ! (4)
        integer(k1) :: j = 9
    end type

    type Container(k2)    ! (4)
        integer, kind            :: k2
        type(Base(k2)), pointer  :: b
        type(Child(k2)), pointer :: c
    end type
end module

program data001
use m
    type(Container(4)) :: c1
    DATA c1 / Container(4)( null(), c=null() ) /

    type(Base(4)), pointer :: b1
    DATA b1 / null() /

    if(associated(c1%b)) error stop 1_4
    if(associated(c1%c)) error stop 2_4
    if(associated(b1)) error stop 3_4
end
