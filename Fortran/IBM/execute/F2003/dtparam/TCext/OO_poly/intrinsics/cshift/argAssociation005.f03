! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/argAssociation005.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 02/02/2005
! PRIMARY FUNCTIONS TESTED   : cshift
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)      j
    end type
end module

program argAssociation005
use m
    class(Base(4)), pointer :: b(:)
    class(Child(4,4)), allocatable :: c(:,:)

    allocate(b(10), SOURCE=(/(Child(4,4)(i,i+1),i=1,10)/))
    allocate(c(2,3), SOURCE=reshape((/(Child(4,4)(i,i+2),i=1,6)/), &
     (/2,3/), (/Child(4,4)(1,1)/), (/2,1/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        class(Base(4)), pointer :: arg1(:)
        class(Child(4,4)), allocatable :: arg2(:,:)

        select type(name1=>cshift(arg1, 5, 1))
            type is (Child(4,4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select

        select type(name1=>cshift(arg2, (/1,2,3/)))
            type is (Child(4,4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 2_4
        end select
    end subroutine
end