! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/unpack/argAssociation005.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : VECTOR or FIELD is a dummy argument. Dummy
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type
end module

program argAssociation005
use m
    class(Base(:,4)), pointer :: b(:)
    class(Child(:,4)), allocatable :: c(:,:)

    allocate(b(10), SOURCE=(/(Child(20,4)(i,i+1),i=1,10)/))
    allocate(c(2,3), SOURCE=reshape((/(Child(20,4)(i,i+2),i=1,5)/), &
     (/2,3/), (/Child(20,4)(1,-1)/), (/2,1/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        class(Base(:,4)), pointer :: arg1(:)
        class(Child(:,4)), allocatable :: arg2(:,:)

        select type(name1=>unpack(arg1(3:9), MOD(arg2%i,2)==1, arg2))
            type is (Child(*,4))
                print *, name1
                print *, shape(name1)
            class default
                error stop 1_4
        end select
    end subroutine
end