! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/eoshift/reshape004.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/04/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : Function return of eoshift is the SOURCE
!                              of reshape. Poly and unlimited poly.
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

program reshape004
use m
    class(AbstractParent(4,:)), pointer :: c1(:,:,:)
    class(*), pointer :: b1(:)

    allocate(c1(2,2,2), SOURCE=reshape((/(Child(4,20)(i,-i),i=1,8)/), &
     (/2,2,2/)))

    select type(name1=>reshape(eoshift(c1, reshape((/1,-2,2,-1/), &
     (/2,2/)), reshape((/(Child(4,20)(i,i+1),i=11,14)/), (/2,2/))), &
     (/3,4/), (/Child(4,20)(-1,-2),Child(4,20)(-3,-4)/), (/2,1/)))
        type is (Child(4,*))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    allocate(b1(8), SOURCE=(/(Base(4,20)(i),i=101,108)/))

    select type(name1=>reshape(eoshift(b1, 2, Base(4,20)(-7)), (/2,3,3/), &
     (/Base(4,20)(-8),Base(4,20)(-9)/), (/1,2,3/)))
        type is (Base(4,*))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
