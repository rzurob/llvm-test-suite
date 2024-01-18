!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! PROGRAMMER                 : Yong Du
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is poly. SHIFT is scalar or array.
!                              BOUNDARY is scalar or array, non
!                              poly or poly. DIM is present or not.
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
        integer :: i = 8
    end type

    type, extends(Base) :: Child
        integer :: j = 9
    end type
end module

program eoshift002
use m
    class(Base), pointer :: b1(:,:,:)
    class(AbstractParent), allocatable :: a1(:,:)

    allocate(b1(3,2,2), SOURCE=reshape((/(Child(i,-i),i=1,12)/), &
     (/3,2,2/)))
    allocate(a1(2,2),SOURCE=reshape((/(Child(i,i+1),i=11,14)/),(/2,2/)))

    select type(name1=>eoshift(b1, -1, a1))
        type is (Child)
            print *, name1
        class default
            error stop 1_4
    end select

    select type(name1=>eoshift(b1, 1, Child(88,-88), 2))
        type is (Child)
            print *, name1
        class default
            error stop 2_4
    end select

    select type(name1=>eoshift(b1,reshape((/1,2,-1,-2/),(/2,2/)),a1,1))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 3_4
    end select
end
