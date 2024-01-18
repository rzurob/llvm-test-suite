!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is a structure component, which
!                              is unlimited poly array. The object
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
        class(*), pointer :: b1(:,:)
    end type
end module

program structureComponent003
use m
    class(Container), allocatable :: c1
    logical :: m1(25)

    allocate(Container::c1)
    allocate(c1%b1(5,4), SOURCE=reshape((/(Base(i),i=1,20)/),(/5,4/)))
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., &
           .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE., &
           .TRUE.,.FALSE.,.FALSE.,.FALSE.,.TRUE., &
           .FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE., &
           .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE./)

    select type(name1=>pack(c1%b1, reshape(m1, (/5,4/))))
        type is (Base)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(c1%b1)
    allocate(c1%b1(4,3), SOURCE=reshape((/(Child(i,-i),i=1,12)/), &
     (/4,3/)))

    select type(name1=>pack(c1%b1, reshape(m1, (/4,3/)), &
     reshape(c1%b1, (/8/))))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
