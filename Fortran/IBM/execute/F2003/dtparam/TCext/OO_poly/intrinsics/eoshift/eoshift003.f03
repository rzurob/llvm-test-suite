! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/eoshift003.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : ARRAY is unlimited poly. SHIFT is scalar
!                              or array. BOUNDARY is scalar or array, non
!                              poly or poly. DIM is present or not.
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

program eoshift003
use m
    class(*), pointer :: b1(:,:,:)
    class(*), allocatable :: a1(:,:)
    class(*), allocatable :: s1(:,:)
    allocate(b1(3,2,2), SOURCE=reshape((/(Child(4,4)(i,-i),i=1,12)/), &
     (/3,2,2/)))
    allocate(a1(2,2),SOURCE=reshape((/(Child(4,4)(i,i+1),i=11,14)/),(/2,2/)))
    allocate(s1(2,2),SOURCE=reshape((/1,-1,1,-2/),(/2,2/)))

    select type(s1)
        type is (integer)
            select type(name1=>eoshift(b1, s1, a1, 1))
                type is (Child(4,4))
                    print *, name1
                    print *, size(name1)
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select
end
