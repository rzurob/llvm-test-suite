!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/24/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Function return of unpack is the SOURCE
!                              of transfer. Poly and unlimited poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent
        integer i
    end type

    type, extends(AbstractParent) :: Base
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program transfer004
use m
    class(*), pointer :: v1(:)
    class(AbstractParent), pointer :: f1(:,:,:)
    class(*), pointer :: b1(:)

    allocate(v1(6), SOURCE=(/(Base(i),i=11,16)/))
    allocate(f1(2,2,2), SOURCE=reshape((/(Base(-i),i=1,8)/), (/2,2,2/)))
    allocate(b1(8), SOURCE=(/(Child(i,-i),i=101,108)/))

    select type(name1=>transfer(unpack(v1, MOD(f1%i,2)==0, f1), b1))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
