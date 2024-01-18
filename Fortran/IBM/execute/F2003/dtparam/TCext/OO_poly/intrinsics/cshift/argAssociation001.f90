! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/argAssociation001.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! PROGRAMMER                 : Yong Du
! DATE                       : 02/02/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is a dummy argument. Dummy
!                              argument is non-pointer, non-allocatable,
!                              non-poly, and is array.
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

program argAssociation001
use m
    type(Base(4)) :: b(10)
    class(Base(4)), pointer :: c(:,:)
    class(Base(4)), pointer :: b1(:,:,:)

    b = (/ (Base(4)(i),i=1,10) /)

    allocate(c(3,2), SOURCE=reshape((/(Child(4)(j=i,i=-i),i=1,6)/),(/3,2/)))

    allocate(b1(2,2,3), SOURCE=reshape((/(Base(4)(i),i=2,13)/),(/2,2,3/)))

    call sub1(b, c, b1)

    contains

    subroutine sub1(arg1, arg2, arg3)
        type(Base(4)) :: arg1(10)
        type(Base(4)) :: arg2(:,:)
        type(Base(4)) :: arg3(3,*)

        print *, cshift(arg1, -2, 1)
        print *, cshift(arg2, (/1,2/), 1)

        associate(name1=>cshift(arg3(:,:4), (/2,-3,4/), 2))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        end associate
    end subroutine
end
