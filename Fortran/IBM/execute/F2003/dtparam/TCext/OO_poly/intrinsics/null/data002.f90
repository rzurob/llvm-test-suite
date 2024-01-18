! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/null/data002.f
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: i = 8
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) :: j = 9
    end type

    type Container(k2)    ! (4)
        integer, kind             :: k2
        class(Base(k2)), pointer  :: b
        class(Child(k2)), pointer :: c
    end type
end module

program data002
use m
    type(Container(4)) :: c1
    DATA c1 / Container(4)( null(), c=null() ) /

    class(Base(4)), pointer :: b1
    DATA b1 / null() /

    if(associated(c1%b)) error stop 1_4
    if(associated(c1%c)) error stop 2_4
    if(associated(b1)) error stop 3_4
end
