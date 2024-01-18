! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/OO_poly/intrinsics/unpack/unpack006.f
! opt variations: -qnok -qnol -qreuse=base

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/22/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : VECTOR is unlimited poly. FIELD is array.
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

program unpack006
use m
    class(*), pointer :: b1(:)
    logical :: m1(3,4)
    class(*), allocatable :: f1(:,:)

    allocate(b1(6), SOURCE=(/(Child(4,20,20,4,20,4)(i,-i),i=1,6)/))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., &
     .FALSE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./), (/3,4/))
    allocate(f1(3,4), SOURCE=reshape((/(Child(4,20,20,4,20,4)(i,-i),i=101,112)/), &
     (/3,4/),ORDER=(/2,1/)))

    select type(name1=>unpack(b1, m1, f1))
        type is (Child(4,*,*,4,*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
