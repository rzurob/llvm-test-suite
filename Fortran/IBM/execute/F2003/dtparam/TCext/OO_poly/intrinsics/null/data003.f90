! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=none -qdeferredlp /tstdev/OO_poly/intrinsics/null/data003.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! PROGRAMMER                 : Yong Du
! DATE                       : 03/01/2005
! PRIMARY FUNCTIONS TESTED   : null
! DRIVER STANZA              : xlf90
! DESCRIPTION                : null is used in a DATA statement.
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

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)   :: j = 9
    end type

    type Container(n1)    ! (20)
        integer, len :: n1
        class(*), pointer :: b
        class(*), pointer :: c
    end type
end module

program data003
use m
    type(Container(20)) :: c1
    DATA c1 / Container(20)( null(), c=null() ) /

    class(*), pointer :: b1
    DATA b1 / null() /

    if(associated(c1%b)) error stop 1_4
    if(associated(c1%c)) error stop 2_4
    if(associated(b1)) error stop 3_4
end
