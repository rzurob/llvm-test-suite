! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all /tstdev/OO_poly/intrinsics/null/typeQuery001.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 03/01/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : Use EXTENDS_TYPE_OF and SAME_TYPE_AS to
!                              check the return value. Non-poly and poly.
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
        integer(k1) :: i = 88
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) :: j = 99
    end type

    type, extends(Base) :: Child1    ! (4,20)
        integer(k1) :: j = 99
    end type
end module

program typeQuery001
use m
    class(AbstractParent(4,20)), allocatable :: b1    ! unallocated
    class(AbstractParent(4,20)), pointer :: b2(:,:,:) ! undefined
    class(Base(4,20)), allocatable :: b3              ! unallocated
    class(Base(4,20)), pointer :: b4(:,:)             ! undefined
    type(Base(4,20)), pointer :: b5                   ! undefined
    type(Child(4,20)), allocatable :: b6(:,:)         ! unallocated

    if(.NOT. extends_type_of(b3, b1)) error stop 1_4
    if(.NOT. extends_type_of(b6, b3)) error stop 2_4

    allocate(Base(4,20)::b1)
    allocate(b2(2,3,2), SOURCE=reshape((/(Child(4,20)(i,i),i=1,12)/), &
     (/2,3,2/)))
    allocate(b3, SOURCE=Child(4,20)(1,2))
    allocate(Child1(4,20)::b4(2,4))
    allocate(b5)
    allocate(b6(3,4))

    if(.NOT. extends_type_of(b2, b1)) error stop 3_4
    if(same_type_as(b2, b1)) error stop 4_4
    if(.NOT. same_type_as(null(b2), null(b1))) error stop 5_4

    if(extends_type_of(b3, b4)) error stop 6_4
    if(same_type_as(b3, b4)) error stop 7_4
    if(.NOT. same_type_as(null(b3), null(b4))) error stop 8_4
    if(.NOT. extends_type_of(b4, null(b3))) error stop 9_4

    if(.NOT. extends_type_of(b6, b5)) error stop 10_4
    if(.NOT. extends_type_of(null(b6), null(b5))) error stop 11_4
end
