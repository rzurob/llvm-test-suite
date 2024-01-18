! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=none -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/array001.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! PROGRAMMER                 : Yong Du
! DATE                       : 01/27/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY or SHIFT is array element or array
!                              section. Poly and unlimited poly.
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

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)      j
    end type
end module

program array001
use m
    class(Base(4)), allocatable :: b1(:,:)
    class(*), pointer :: u1(:,:)
    integer :: i1(5,5)

    allocate(b1(4,5), SOURCE=reshape((/(Child(4,4)(i,-i),i=1,20)/),(/4,5/)))
    allocate(u1(3,4), SOURCE=reshape((/(Base(4)(i),i=1,12)/),(/3,4/)))
    i1 = reshape((/(i,i=1,25)/), (/5,5/))

    select type(name1=>cshift(b1(2:3,2:4), i1(3, 2), 2))
        type is (Child(4,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>cshift(u1(:,2:4), i1(2:4,2)))
        type is (Base(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
