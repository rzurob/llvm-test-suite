!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! PROGRAMMER                 : Yong Du
! DATE                       : 02/02/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is a dummy argument. Dummy
!                              argument is a pointer or allocatable,
!                              unlimited poly, and is array.
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

program argAssociation006
use m
    class(*), pointer :: b(:,:,:)
    class(*), allocatable :: c(:,:)

    allocate(b(3,4,2), SOURCE=reshape((/(Child(i,i+1),i=1,24)/), &
     (/3,4,2/)))
    allocate(c(2,3), SOURCE=reshape((/(Base(i),i=3,8)/), &
     (/2,3/), (/Base(1)/), (/2,1/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        class(*), pointer :: arg1(:,:,:)
        class(*), allocatable :: arg2(:,:)

        select type(name1=>cshift(arg1,reshape((/(i,i=1,8)/),(/4,2/))))
            type is (Child)
                print *, name1
                print *, shape(name1)
            class default
                error stop 1_4
        end select

        select type(name1=>cshift(arg2, (/2,1/), 2))
            type is (Base)
                print *, name1
                print *, shape(name1)
            class default
                error stop 2_4
        end select
    end subroutine
end
