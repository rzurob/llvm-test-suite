! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/spread002.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/04/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : Function return of eoshift is the SOURCE
!                              of spread. Poly and unlimited poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(n1)    ! (20)
        integer, len :: n1
    end type

    type, extends(AbstractParent) :: Base(k1)    ! (20,4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2)    ! (20,4,4)
        integer, kind :: k2
        integer(k2)      j
    end type
end module

program spread002
use m
    class(AbstractParent(:)), pointer :: c1(:,:)
    class(*), pointer :: b1(:,:)

    allocate(c1(4,2), SOURCE=reshape((/(Child(20,4,4)(i,-i),i=1,8)/),(/4,2/)))
    allocate(b1(3,5), SOURCE=reshape((/(Base(20,4)(i),i=101,115)/),(/3,5/)))

    select type(name1=>spread(eoshift(c1, (/-2,3/), &
     (/Child(20,4,4)(17,-17),Child(20,4,4)(18,-18)/)), 3, 2))
        type is (Child(*,4,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>spread(eoshift(b1, (/1,-2,3/), &
     (/Base(20,4)(1),Base(20,4)(2),Base(20,4)(3)/), 2), 1, 2))
        type is (Base(*,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
