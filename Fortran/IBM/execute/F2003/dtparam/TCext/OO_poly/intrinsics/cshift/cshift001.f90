! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/cshift001.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! PROGRAMMER                 : Yong Du
! DATE                       : 01/27/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is non-poly. SHIFT is scalar or
!                              array. DIM is present or not.
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
        integer(k1)   :: i = 9
    end type
end module

program cshift001
use m
    type(Base(4)) :: b1(4,3)
    b1 = reshape((/(Base(4)(i),i=1,12)/), (/4,3/))

    print *, cshift(b1, -1, 1)
    print *, cshift(b1, -1, 2)
    print *, cshift(b1, (/3,-6,6/))
    print *, cshift(b1, (/2,-5,7,-1/), 2)
end
