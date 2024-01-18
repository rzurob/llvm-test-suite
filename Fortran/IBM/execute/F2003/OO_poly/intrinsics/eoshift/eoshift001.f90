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
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer :: i = 9
    end type
end module

program eoshift001
use m
    type(Base) :: b1(4,3)
    class(AbstractParent), pointer :: a1(:)

    b1 = reshape((/(Base(i),i=1,12)/), (/4,3/))
    allocate(a1(4), SOURCE=(/(Base(i),i=14,17)/))

    print *, eoshift(b1, -1, a1(2:))
    print *, eoshift(b1, (/2,-1,1,-2/), Base(-88), 2)
    print *, eoshift(b1, 1, a1, 2)
    print *, eoshift(b1, (/1,-1,1/), a1(1:3))
end
