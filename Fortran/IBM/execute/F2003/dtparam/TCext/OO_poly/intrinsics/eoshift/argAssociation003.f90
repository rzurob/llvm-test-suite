! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=none -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/argAssociation003.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : ARRAY is a dummy argument. Dummy
!                              argument is non-pointer, non-allocatable,
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

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)      j
    end type

    contains

    subroutine sub1(arg1, arg2, arg3, arg4, arg5)
        class(*) :: arg1(10)
        class(*) :: arg2(:,:)
        class(*) :: arg3(:)
        class(*) :: arg4(2,2)
        class(*) :: arg5(:,:,:)

        select type(name1=>eoshift(arg5, reshape((/1,-2,1,-1/), &
         (/2,2/)), reshape(arg1,(/2,2/))))
            type is (Base(4))
                print *, name1
            class default
                error stop 1_4
        end select

        select type(name1=>eoshift(arg2, (/1,-1/), arg3(4:5), 2))
            type is (Child(4,4))
                print *, name1
            class default
                error stop 2_4
        end select

        select type(name1=>eoshift(arg4,(/-1,1/),(/arg2(1,2),arg3(5)/)))
            type is (Child(4,4))
                print *, name1
            class default
                error stop 3_4
        end select
    end subroutine
end module

program argAssociation003
use m
    type(Base(4)) :: b1(10)
    type(Child(4,4)) :: c1(2,3)
    class(Base(4)), pointer :: b2(:)
    class(Child(4,4)), allocatable :: c2(:,:)
    class(*), allocatable :: u1(:,:,:)

    b1 = (/ (Base(4)(i),i=1,10) /)
    c1 = reshape((/(Child(4,4)(i,i+1),i=5,15,2)/),(/2,3/))
    allocate(b2(6), SOURCE=(/(Child(4,4)(i,i+1),i=2,7)/))
    allocate(c2(2,2), SOURCE=reshape((/(Child(4,4)(j=i-1,i=i), &
     i=12,15)/), (/2,2/)))
    allocate(u1(3,2,2), SOURCE=reshape((/(Base(4)(i),i=4,15)/),(/3,2,2/)))

    call sub1(b1, c1, b2, c2, u1)
end
