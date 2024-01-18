!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! PROGRAMMER                 : Yong Du
! DATE                       : 02/04/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is function return of reshape.
!                              Poly and unlimited poly.
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

program reshape003
use m
    class(AbstractParent), pointer :: c1(:,:,:)
    class(*), pointer :: b1(:)

    allocate(c1(2,2,2), SOURCE=reshape((/(Child(i,i-1),i=101,108)/), &
     (/2,2,2/)))

    select type(name1=>eoshift(reshape(c1,(/3,4/),(/Child(-1,-2), &
     Child(-3,-4)/),(/2,1/)),(/1,-2,2,-1/),Child(8,9),1))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    allocate(b1(12), SOURCE=(/(Child(i,-i),i=1,12)/))

    select type(name1=>eoshift(reshape(b1, (/3,2,2/)), &
     reshape((/1,-2,-3,-1/), (/2,2/)), &
     reshape((/(Child(i,i+1),i=10,13)/), (/2,2/))))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
