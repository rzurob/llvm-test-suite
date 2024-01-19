!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/22/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : ARRAY is a dummy argument. Dummy
!                              argument is a pointer or allocatable,
!                              poly, and is array.
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

program argAssociation005
use m
    class(Base), pointer :: b(:)
    class(Child), allocatable :: c(:,:)

    allocate(b(10), SOURCE=(/(Child(i,i+1),i=1,10)/))
    allocate(c(2,3), SOURCE=reshape((/(Child(i,i+2),i=1,5)/), &
     (/2,3/), (/Child(1,-1)/), (/2,1/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        class(Base), pointer :: arg1(:)
        class(Child), allocatable :: arg2(:,:)

        select type(name1=>pack(arg2, MOD(arg2%i,2)==1, arg1(3:9)))
            type is (Child)
                print *, name1
                print *, shape(name1)
            class default
                error stop 1_4
        end select
    end subroutine
end
