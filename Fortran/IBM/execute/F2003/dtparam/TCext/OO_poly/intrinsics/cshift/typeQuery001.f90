! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/typeQuery001.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! PROGRAMMER                 : Yong Du
! DATE                       : 01/31/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DRIVER STANZA              : xlf90
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

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)   :: j = 99
    end type
end module

program typeQuery001
use m
    class(Base(4)), pointer :: b1(:,:,:)
    class(Base(4)), allocatable :: b2(:,:)
    class(*), pointer :: b3(:) => null()

    allocate(b1(3,2,2), SOURCE=reshape((/(Child(4,4)(i,-i),i=1,12)/), &
     (/3,2,2/)))
    allocate(b2(2,3), SOURCE=reshape((/(Child(4,4)(i,i),i=1,6)/), (/2,3/)))
    allocate(Base(4)::b3(3))

    if(.NOT. same_type_as(cshift(b1,3,2), Child(4,4)(1,1))) error stop 1_4

    if(.NOT. extends_type_of(cshift(b2,(/1,2,3/),1), Base(4)(1))) &
     error stop 2_4

    if(.NOT. same_type_as(cshift(b3,-5), Base(4)(1))) error stop 3_4
end
