!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
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
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    type Container
        class(AbstractParent), pointer :: b1(:,:)
        class(AbstractParent), allocatable :: v1(:)
    end type
end module

program structureComponent002
use m
    type(Container) :: c1
    logical :: m1(25)

    allocate(c1%b1(5,4), SOURCE=reshape((/(Base(i),i=1,20)/),(/5,4/)))
    allocate(c1%v1(10), SOURCE=(/(Base(-i),i=101,110)/))
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., &
           .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE., &
           .TRUE.,.FALSE.,.FALSE.,.FALSE.,.TRUE., &
           .FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE., &
           .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE./)

    select type(name1=>unpack(c1%v1, reshape(m1, (/5,4/)), c1%b1))
        type is (Base)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(c1%b1, c1%v1)
    allocate(c1%b1(4,3), SOURCE=reshape((/(Child(i,-i),i=1,12)/), &
     (/4,3/)))
    allocate(c1%v1(10), SOURCE=(/(Child(-i,i),i=101,110)/))

    select type(name1=>unpack(c1%v1, reshape(m1, (/4,3/)), c1%b1))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
