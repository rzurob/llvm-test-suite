! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/spread001.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 02/01/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : ARRAY is function return of spread.
!                              Poly and unlimited poly.
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
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type
end module

program spread001
use m
    class(AbstractParent(4,:)), pointer :: c1(:,:)
    class(*), pointer :: b1(:,:)

    allocate(c1(4,2), SOURCE=reshape((/(Child(4,20)(i,-i),i=1,8)/),(/4,2/)))
    allocate(b1(3,5), SOURCE=reshape((/(Base(4,20)(i),i=1,15)/),(/3,5/)))

    select type(name1=>cshift(spread(c1,3,3), &
     reshape((/1,-2,3,-4,5,-6/),(/2,3/)), 1))
        type is (Child(4,*))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>cshift(spread(b1,3,2), &
     reshape((/-1,2,-3,4,-5,6/),(/3,2/)), 2))
        type is (Base(4,*))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
