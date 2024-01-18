! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/array001.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! PROGRAMMER                 : Yong Du
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY, SHIFT, or BOUNDARY is array
!                              element or array section. Poly and
!                              unlimited poly.
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

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program array001
use m
    class(Base(4)), allocatable :: b1(:,:)
    class(*), pointer :: a1(:,:)
    integer :: i1(5,5)

    allocate(b1(4,5),SOURCE=reshape((/(Child(4)(i,-i),i=1,20)/),(/4,5/)))
    allocate(a1(3,4),SOURCE=reshape((/(Child(4)(i,i+1),i=1,12)/),(/3,4/)))
    i1 = reshape((/(i,i=1,25)/), (/5,5/), (/-1/), (/2,1/))

    select type(name1=>eoshift(b1(2:4,2:), i1(1,:3), a1(2,2:), 2))
        type is (Child(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
