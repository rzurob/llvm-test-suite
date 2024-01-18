! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/intrinsics/unpack/diagnose001.f
! opt variations: -qnol

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/22/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Diagnose test case. VECTOR shall have
!                              rank one.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: i = 9
    end type
end module

program diagnose001
use m
    type(Base(20,4)) :: b1

    print *, unpack(b1, (/.TRUE.,.FALSE./), (/Base(20,4)(1),Base(20,4)(2)/))
end
