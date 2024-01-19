! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/null/structureComponent002.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 03/03/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : MOLD is a structure component, scalar or
!                              array. Poly.
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
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    type Container(k2)    ! (4)
        integer, kind                :: k2
        class(Base(k2)), pointer     :: b1
        class(Base(k2)), allocatable :: b2(:,:)
    end type
end module

program structureComponent002
use m
    type(Container(4)) :: c1(3)
    type(Container(4)) :: c2(3)

    do i=1,3
        allocate(c1(i)%b1, SOURCE=Child(4)(i,i))
        allocate(c1(i)%b2(i,i), SOURCE=Child(4)(-i,-i))
    end do

    do i=1,3
        if(.NOT. associated(c1(i)%b1)) error stop 1_4
        if(.NOT. allocated(c1(i)%b2)) error stop 2_4
        if(.NOT. same_type_as(c1(i)%b1, Child(4)(1,1))) error stop 3_4
        if(.NOT. same_type_as(c1(i)%b2, Child(4)(1,1))) error stop 4_4
    end do

    do i=1,3
        c1(MOD(i,3)+1)%b1 => null(c1(i)%b1)
    end do

    do i=1,3
        if(associated(c1(i)%b1)) error stop 5_4
        if(.NOT. same_type_as(c1(i)%b1, Base(4)(1))) error stop 6_4
    end do

    do i=1,3
        c2(i) = Container(4)(null(c1(i)%b1), null(c1(i)%b2))
    end do

    do i=1,3
        if(associated(c2(i)%b1)) error stop 7_4
        if(allocated(c2(i)%b2)) error stop 8_4
        if(.NOT. same_type_as(c2(i)%b1, Base(4)(1))) error stop 9_4
        if(.NOT. same_type_as(c2(i)%b2, Base(4)(1))) error stop 10_4
    end do
end
