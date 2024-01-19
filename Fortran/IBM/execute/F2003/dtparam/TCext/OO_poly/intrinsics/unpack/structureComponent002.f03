! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/unpack/structureComponent002.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : VECTOR or FIELD is a structure component,
!                              which is poly array. The object
!                              containing the component is a scalar.
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

    type Container(k4)    ! (4)
        integer, kind                            :: k4
        class(AbstractParent(k4,:)), pointer     :: b1(:,:)
        class(AbstractParent(k4,:)), allocatable :: v1(:)
    end type
end module

program structureComponent002
use m
    type(Container(4)) :: c1
    logical :: m1(25)

    allocate(c1%b1(5,4), SOURCE=reshape((/(Base(4,20,20,4)(i),i=1,20)/),(/5,4/)))
    allocate(c1%v1(10), SOURCE=(/(Base(4,20,20,4)(-i),i=101,110)/))
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., &
           .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE., &
           .TRUE.,.FALSE.,.FALSE.,.FALSE.,.TRUE., &
           .FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE., &
           .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE./)

    select type(name1=>unpack(c1%v1, reshape(m1, (/5,4/)), c1%b1))
        type is (Base(4,*,*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(c1%b1, c1%v1)
    allocate(c1%b1(4,3), SOURCE=reshape((/(Child(4,20,20,4,20,4)(i,-i),i=1,12)/), &
     (/4,3/)))
    allocate(c1%v1(10), SOURCE=(/(Child(4,20,20,4,20,4)(-i,i),i=101,110)/))

    select type(name1=>unpack(c1%v1, reshape(m1, (/4,3/)), c1%b1))
        type is (Child(4,*,*,4,*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
