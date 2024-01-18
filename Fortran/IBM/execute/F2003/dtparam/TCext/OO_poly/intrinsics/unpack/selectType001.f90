! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/unpack/selectType001.f
! opt variations: -qnok -ql -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/23/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : VECTOR or FIELD is an associate name of
!                              a select type construct. Associate name
!                              is poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program selectType001
use m
    class(AbstractParent(4)), pointer :: ap1(:,:) => null()
    class(*), allocatable :: v1(:)
    logical :: m1(5,5)

    allocate(ap1(3,4),SOURCE=reshape((/(Child(4)(i,-i),i=1,20)/),(/3,4/)))
    allocate(v1(12),SOURCE=(/(Child(4)(i,i+1),i=11,22)/))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE., &
     .FALSE.,.TRUE./), (/5,5/), (/.FALSE.,.TRUE./))

    select type(name1=>v1(4:10))
        class is (Child(4))
            select type(name2=>unpack(name1,m1(2:4,2:),ap1))
                type is (Child(4))
                    print *, name2
                    print *, shape(name2)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select
end
