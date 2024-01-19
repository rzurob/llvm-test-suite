! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/unpack/argumentKeyword001.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/23/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Actual arguments are speficied using
!                              argument keywords. Poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: i = 8
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) :: j = 9
    end type
end module

program argumentKeyword001
use m
    class(Base(:,4)), pointer :: v1(:)
    class(Base(:,4)), pointer :: b1(:,:,:)
    allocate(b1(3,2,2), SOURCE=reshape((/(Child(20,4)(i,-i),i=1,12)/), &
     (/3,2,2/)))
    allocate(v1(8), SOURCE=(/(Child(20,4)(i,-i),i=101,108)/))

    select type(name1=>unpack(MASK=(MOD(b1%i,2)==0),FIELD=b1,VECTOR=v1))
        type is (Child(*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
