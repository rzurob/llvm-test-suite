! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/null/selectType003.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! PROGRAMMER                 : Yong Du
! DATE                       : 03/02/2005
! PRIMARY FUNCTIONS TESTED   : null
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Return of null is the selector of a
!                              select type construct. MOLD is unlimited
!                              poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(n1)    ! (20)
        integer, len :: n1
    end type

    type, extends(AbstractParent) :: Base(k1)    ! (20,4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2)    ! (20,4,4)
        integer, kind :: k2
        integer(k2)      j
    end type
end module

program selectType003
use m
    class(*), allocatable :: u1(:,:)
    allocate(u1(4,5),SOURCE=reshape((/(Child(20,4,4)(i,-i),i=1,20)/),(/4,5/)))

    select type(name1=>null(u1))
        type is (Child(*,4,4))
            error stop 1_4
        type is (Base(*,4))
            error stop 2_4
        class is (AbstractParent(*))
            error stop 3_4
        class default
            ! should come here, but do nothing
    end select
end
