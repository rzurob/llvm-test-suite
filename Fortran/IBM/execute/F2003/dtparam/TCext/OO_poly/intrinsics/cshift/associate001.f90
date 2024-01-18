! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/associate001.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! PROGRAMMER                 : Yong Du
! DATE                       : 01/27/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY or SHIFT is an associate name.
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

program associate001
use m
    class(AbstractParent(:)), pointer :: ap1(:,:) => null()
    integer :: i1(5,5)

    allocate(ap1(4,5), SOURCE=reshape((/(Child(20,4,4)(i,-i),i=1,20)/), (/4,5/)))
    i1 = reshape((/(i,i=1,25)/), (/5,5/))

    associate(name1=>ap1(2:,:4), name2=>i1(2,:3))
        select type(name3=>cshift(name1,name2,2))
            type is (Child(*,4,4))
                print *, name3
                print *, size(name3)
                print *, shape(name3)
            class default
                error stop 1_4
        end select
    end associate
end
