!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! PROGRAMMER                 : Yong Du
! DATE                       : 01/31/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is an associate name of a select
!                              type construct.
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
end module

program selectType001
use m
    class(AbstractParent), pointer :: ap1(:,:) => null()
    integer :: i1(5,5)

    allocate(ap1(4,5), SOURCE=reshape((/(Child(i,-i),i=1,20)/), (/4,5/)))
    i1 = reshape((/(i,i=1,25)/), (/5,5/))

    select type(name1=>ap1(2:,:4))
        type is (Child)
            associate(name2=>cshift(name1,i1(2,:3),2))
                if(.NOT. same_type_as(name2, Child(1,1))) error stop 1_4
                print *, name2
                print *, size(name2)
                print *, shape(name2)
            end associate
        class default
            error stop 2_4
    end select
end
