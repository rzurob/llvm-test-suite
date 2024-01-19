! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/functionReturn002.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 02/02/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : Diagnostic test case. When return value
!                              of cshift is poly, it shall not be
!                              processed by regular IO.
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
        integer(k1)      i
    end type
end module

program functionReturn002
use m
    class(Base(4)), allocatable :: b1(:,:)

    allocate(b1(3,4), SOURCE=reshape((/(Base(4)(i),i=1,12)/), (/3,4/)))

    print *, cshift(b1, 1)
end
