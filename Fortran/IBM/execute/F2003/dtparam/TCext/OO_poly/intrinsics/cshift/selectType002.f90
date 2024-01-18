! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/selectType002.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! PROGRAMMER                 : Yong Du
! DATE                       : 01/31/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is an associate name of a select
!                              type construct. Associate name is
!                              polymorphic.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type
end module

program selectType002
use m
    class(AbstractParent(4,:)), pointer :: ap1(:,:) => null()
    integer :: i1(5,5)

    allocate(ap1(4,5), SOURCE=reshape((/(Child(4,20)(i,-i),i=1,20)/), (/4,5/)))
    i1 = reshape((/(i,i=1,25)/), (/5,5/))

    select type(name1=>ap1(2:,:4))
        class is (Child(4,*))
            select type(name2=>cshift(name1,i1(2,:3),2))
                type is (Child(4,*))
                    print *, name2
                    print *, size(name2)
                    print *, shape(name2)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select
end
