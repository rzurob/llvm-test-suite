! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/typeQuery001.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/03/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: i = 88
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) :: j = 99
    end type
end module

program typeQuery001
use m
    class(Base(4)), pointer :: b1(:,:,:)
    class(Base(4)), allocatable :: b2(:,:)
    class(*), pointer :: b3(:) => null()

    allocate(b1(3,2,2), SOURCE=reshape((/(Child(4)(i,-i),i=1,12)/), &
     (/3,2,2/)))
    allocate(b2(2,3), SOURCE=reshape((/(Child(4)(i,i),i=1,6)/), (/2,3/)))
    allocate(Base(4)::b3(3))

    if(.NOT. same_type_as(eoshift(b1,3,Child(4)(1,2)), Child(4)(1,1))) &
     error stop 1_4

    if(.NOT. extends_type_of(eoshift(b2,(/1,2,3/),Base(4)(2),1), &
     Base(4)(1))) error stop 2_4

    if(.NOT. same_type_as(eoshift(b3,-5,Child(4)(1,2)), Base(4)(1))) &
     error stop 3_4
end
