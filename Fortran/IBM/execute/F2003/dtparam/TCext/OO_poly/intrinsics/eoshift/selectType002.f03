! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/selectType002.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : ARRAY is an associate name of a select
!                              type construct. Associate name is
!                              polymorphic.
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

program selectType002
use m
    class(AbstractParent(4,:)), pointer :: ap1(:,:) => null()
    integer :: i1(5,5)
    class(AbstractParent(4,:)), allocatable :: b1(:,:)

    allocate(ap1(4,5),SOURCE=reshape((/(Child(4,20)(i,-i),i=1,20)/),(/4,5/)))
    i1 = reshape((/(i,i=1,25)/), (/5,5/))
    allocate(b1(3,3),SOURCE=reshape((/(Child(4,20)(i,i+1),i=11,19)/),(/3,3/)))

    select type(name1=>ap1(2:,:4))
        class is (Child(4,*))
            select type(name2=>eoshift(name1,i1(2:4,1),b1(2,:),2))
                type is (Child(4,*))
                    print *, name2
                    print *, size(name2)
                    print *, shape(name2)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select
end