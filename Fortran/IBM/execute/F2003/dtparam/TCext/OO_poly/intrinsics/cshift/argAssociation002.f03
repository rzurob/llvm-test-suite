! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/argAssociation002.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 02/02/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : ARRAY is a dummy argument. Dummy
!                              argument is non-pointer, non-allocatable,
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

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        class(Base(4)) :: arg1(10)
        class(Base(4)) :: arg2(:,:)
        class(Base(4)) :: arg3(:)
        class(Base(4)) :: arg4(2,2)

        select type(name1=>cshift(arg1, 6))
            type is (Base(4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select

        select type(name1=>cshift(arg2, (/2,-2/), 2))
            type is (Child(4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 2_4
        end select

        select type(name1=>cshift(arg3, 4, 1))
            type is (Child(4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 3_4
        end select

        select type(name1=>cshift(arg4, (/1,2/), 2))
            type is (Child(4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 4_4
        end select
    end subroutine
end module

program argAssociation002
use m
    type(Base(4)) :: b1(10)
    type(Child(4)) :: c1(2,3)
    class(Base(4)), pointer :: b2(:)
    class(Child(4)), allocatable :: c2(:,:)

    b1 = (/ (Base(4)(i),i=1,10) /)
    c1 = reshape((/(Child(4)(i,i+1),i=5,15,2)/),(/2,3/))
    allocate(b2(6), SOURCE=(/(Child(4)(i,i+1),i=2,7)/))
    allocate(c2(2,2), SOURCE=reshape((/(Child(4)(j=i-1,i=i), &
     i=12,15)/), (/2,2/)))

    call sub1(b1, c1, b2, c2)
end