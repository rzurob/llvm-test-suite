!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 02/02/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : ARRAY is a dummy argument. Dummy
!                              argument is non-pointer, non-allocatable,
!                              non-poly, and is array.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program argAssociation001
use m
    type(Base) :: b(10)
    class(Base), pointer :: c(:,:)
    class(Base), pointer :: b1(:,:,:)

    b = (/ (Base(i),i=1,10) /)

    allocate(c(3,2), SOURCE=reshape((/(Child(j=i,i=-i),i=1,6)/),(/3,2/)))

    allocate(b1(2,2,3), SOURCE=reshape((/(Base(i),i=2,13)/),(/2,2,3/)))

    call sub1(b, c, b1)

    contains

    subroutine sub1(arg1, arg2, arg3)
        type(Base) :: arg1(10)
        type(Base) :: arg2(:,:)
        type(Base) :: arg3(3,*)

        print *, cshift(arg1, -2, 1)
        print *, cshift(arg2, (/1,2/), 1)

        associate(name1=>cshift(arg3(:,:4), (/2,-3,4/), 2))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        end associate
    end subroutine
end
