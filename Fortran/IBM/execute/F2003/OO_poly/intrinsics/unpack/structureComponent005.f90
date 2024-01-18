!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : VECTOR or FIELD is a structure component,
!                              which is a scalar. The object containing
!                              the component is an array and is poly.
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
        type(Base) :: b1
        integer j
    end type
end module

program structureComponent005
use m
    class(AbstractParent), allocatable :: c1(:,:,:)
    class(*), pointer :: v1(:)
    logical :: m1(12)

    allocate(c1(2,2,3), SOURCE=reshape((/(Child(i,Base(-i),-i), &
     i=101,112)/), (/2,2,3/)))
    allocate(v1(8), SOURCE=(/(Base(i),i=1,8)/))
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., &
     .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./)

    select type(c1)
        type is (Child)
            select type(name1=>unpack(v1, reshape(m1,(/2,2,3/)), c1%b1))
                type is (Base)
                    print *, name1
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select
end
