! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/intrinsics/unpack/diagnose002.f
! opt variations: -ql

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/22/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Diagnose test case. MASK shall be of
!                              type logical.
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

program diagnose002
use m
    class(Base(4)), pointer :: b1(:)
    allocate(Base(4)::b1(2))

    print *, unpack(b1, (/1,0/), (/Base(4)(1),Base(4)(2)/))
end
