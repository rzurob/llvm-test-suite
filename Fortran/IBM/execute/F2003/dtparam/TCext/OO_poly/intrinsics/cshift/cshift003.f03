! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=none -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/cshift003.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 01/27/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : ARRAY is unlimited poly. SHIFT is scalar
!                              or array. DIM is present or not.
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
        integer(k1)   :: i = 8
    end type

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)   :: j = 9
    end type
end module

program cshift003
use m
    class(*), allocatable :: s1
    class(*), pointer :: b1(:,:,:)
    allocate(b1(3,2,2), SOURCE=reshape((/(Child(4,4)(i,-i),i=1,12)/), &
     (/3,2,2/)))
    allocate(s1, SOURCE=3)

    select type(s1)
        type is (integer)
            select type(name1=>cshift(b1, s1, 2))
                type is (Child(4,4))
                    print *, name1
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select

    select type(name1=>cshift(b1,reshape((/1,2,-3,-4,5,-6/),(/3,2/)),3))
        type is (Child(4,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 3_4
    end select
end
