! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/unpack/typeQuery001.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/23/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Use EXTENDS_TYPE_OF and SAME_TYPE_AS to
!                              check the return value.
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
        integer(k2)   :: i = 88
    end type

    type, extends(Base) :: Child(n3,k3)    ! (4,20,20,4,20,4)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)   :: j = 99
    end type
end module

program typeQuery001
use m
    class(AbstractParent(4,:)), pointer :: b1(:)
    class(AbstractParent(4,:)), allocatable :: b2(:,:)
    class(*), pointer :: b3(:) => null()
    logical :: m1(6)

    allocate(b1(4), SOURCE=(/(Child(4,20,20,4,20,4)(i,-i),i=1,4)/))
    allocate(b2(2,3), SOURCE=reshape((/(Child(4,20,20,4,20,4)(i,i),i=1,6)/), (/2,3/)))
    allocate(b3(5), SOURCE=(/(Child(4,20,20,4,20,4)(-i,i),i=11,15)/))
    m1 = (/.FALSE.,.TRUE.,.FALSE.,.FALSE.,.TRUE.,.TRUE./)

    if(.NOT. same_type_as(unpack(b1,reshape(m1,(/2,3/)),b2), &
     Child(4,20,20,4,20,4)(1,1))) error stop 1_4

    if(.NOT. extends_type_of(unpack(b3,reshape(m1,(/2,3/), &
     ORDER=(/2,1/)),b2), Child(4,20,20,4,20,4)(1,1))) error stop 2_4
end
