! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/pack/array001.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY, MASK, or VECTOR is array
!                              element or array section. Poly and
!                              unlimited poly.
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

program array001
use m
    class(Base(:,4)), allocatable :: b1(:,:)
    class(*), pointer :: v1(:)
    logical :: m1(5,5)

    allocate(b1(4,5),SOURCE=reshape((/(Child(20,4)(i,-i),i=1,20)/),(/4,5/)))
    allocate(v1(14),SOURCE=(/(Child(20,4)(i,i+1),i=1,14)/))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE., &
     .FALSE.,.TRUE./), (/5,5/), (/.FALSE.,.TRUE./))

    select type(name1=>pack(b1(2:4,2:), m1(2:4,:4), v1(3:12)))
        type is (Child(*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
