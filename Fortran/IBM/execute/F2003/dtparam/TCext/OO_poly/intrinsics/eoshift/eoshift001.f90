! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/eoshift001.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! PROGRAMMER                 : Yong Du
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is non-poly. SHIFT is scalar or
!                              array. BOUNDARY is scalar or array, non
!                              poly or poly. DIM is present or not.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) :: i = 9
    end type
end module

program eoshift001
use m
    type(Base(4,20)) :: b1(4,3)
    class(AbstractParent(4,:)), pointer :: a1(:)

    b1 = reshape((/(Base(4,20)(i),i=1,12)/), (/4,3/))
    allocate(a1(4), SOURCE=(/(Base(4,20)(i),i=14,17)/))

    print *, eoshift(b1, -1, a1(2:))
    print *, eoshift(b1, (/2,-1,1,-2/), Base(4,20)(-88), 2)
    print *, eoshift(b1, 1, a1, 2)
    print *, eoshift(b1, (/1,-1,1/), a1(1:3))
end
