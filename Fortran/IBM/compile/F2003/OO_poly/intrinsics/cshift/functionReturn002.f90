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
    type Base
        integer i
    end type
end module

program functionReturn002
use m
    class(Base), allocatable :: b1(:,:)

    allocate(b1(3,4), SOURCE=reshape((/(Base(i),i=1,12)/), (/3,4/)))

    print *, cshift(b1, 1)
end
