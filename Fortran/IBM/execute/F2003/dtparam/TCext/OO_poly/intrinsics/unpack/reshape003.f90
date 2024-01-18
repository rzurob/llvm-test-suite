! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/unpack/reshape003.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/24/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : VECTOR or FIELD is function return of
!                              reshape. Poly and unlimited poly.
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

    type, extends(AbstractParent) :: Base(n2,k2)    ! (4,20,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
    end type

    type, extends(Base) :: Child(n3,k3)    ! (4,20,20,4,20,4)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)      j
    end type
end module

program reshape003
use m
    class(*), pointer :: v1(:,:)
    class(AbstractParent(4,:)), pointer :: f1(:)
    logical :: m1(12)

    allocate(v1(2,4),SOURCE=reshape((/(Child(4,20,20,4,20,4)(i,-i),i=11,18)/),(/2,4/)))
    allocate(f1(15), SOURCE=(/(Child(4,20,20,4,20,4)(i,-i),i=101,115)/))
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., &
     .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE./)

    select type(name1=>unpack(reshape(v1,(/8/)), reshape(m1, (/3,4/)), &
     reshape(f1,(/3,4/),(/Child(4,20,20,4,20,4)(-1,-2), Child(4,20,20,4,20,4)(-3,-4)/),(/2,1/))))
        type is (Child(4,*,*,4,*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
