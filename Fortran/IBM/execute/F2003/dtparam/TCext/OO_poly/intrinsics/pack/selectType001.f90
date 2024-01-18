! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/pack/selectType001.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : ARRAY is an associate name of a select
!                              type construct. Associate name is non
!                              poly.
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

    type, extends(AbstractParent) :: Base(n2,k2)    ! (4,20,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
    end type

    type, extends(Base) :: Child(n3,k3)    ! (4,20,20,4,20,4)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)      j
    end type
end module

program selectType001
use m
    class(AbstractParent(4,:)), pointer :: ap1(:,:) => null()
    class(*), allocatable :: v1(:)
    logical :: m1(5,5)

    allocate(ap1(4,5),SOURCE=reshape((/(Child(4,20,20,4,20,4)(i,-i),i=1,20)/),(/4,5/)))
    allocate(v1(10),SOURCE=(/(Child(4,20,20,4,20,4)(i,i+1),i=11,20)/))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE., &
     .FALSE.,.TRUE./), (/5,5/), (/.FALSE.,.TRUE./))

    select type(name1=>ap1(2:,:4))
        type is (Child(4,*,*,4,*,4))
            associate(name2=>pack(name1,m1(:3,2:),v1))
                if(.NOT. same_type_as(name2, Child(4,20,20,4,20,4)(1,1))) error stop 1_4
                print *, name2
                print *, shape(name2)
            end associate
        class default
            error stop 2_4
    end select
end
