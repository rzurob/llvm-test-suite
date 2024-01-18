! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/argumentKeyword001.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! PROGRAMMER                 : Yong Du
! DATE                       : 01/27/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Actual arguments are speficied using
!                              argument keywords.
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
end module

program argumentKeyword001
use m
    class(Base(4)), pointer :: b1(:,:,:)
    allocate(b1(3,2,2), SOURCE=reshape((/(Child(4)(i,-i),i=1,12)/), &
     (/3,2,2/)))

    select type(name1=>cshift(SHIFT=-1, ARRAY=b1))
        type is (Child(4))
            print *, name1
        class default
            error stop 1_4
    end select

    select type(name1=>cshift(b1, DIM=2, SHIFT=3))
        type is (Child(4))
            print *, name1
        class default
            error stop 2_4
    end select

    select type(name1=>cshift(SHIFT=reshape((/1,2,-3,-4,5,-6/), &
     (/3,2/)), ARRAY=b1, DIM=3))
        type is (Child(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 3_4
    end select
end
