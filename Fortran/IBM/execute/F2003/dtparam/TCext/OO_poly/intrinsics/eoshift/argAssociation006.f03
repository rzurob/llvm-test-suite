! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/argAssociation006.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program argAssociation006
use m
    class(*), pointer :: b(:,:,:)
    class(*), allocatable :: c(:,:)

    allocate(b(3,4,2), SOURCE=reshape((/(Child(4)(i,i+1),i=1,24)/), &
     (/3,4,2/)))
    allocate(c(3,2), SOURCE=reshape((/(Child(4)(i,-i),i=3,8)/), &
     (/3,2/), (/Child(4)(1,1)/), (/2,1/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        class(*), pointer :: arg1(:,:,:)
        class(*), allocatable :: arg2(:,:)

        select type(name1=>eoshift(arg1,reshape((/-1,1,-2,2,1,-1/), &
         (/3,2/)), arg2, 2))
            type is (Child(4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select
    end subroutine
end
