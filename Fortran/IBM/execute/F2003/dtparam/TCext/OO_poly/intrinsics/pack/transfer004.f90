! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/intrinsics/pack/transfer004.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/18/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Function return of pack is the SOURCE
!                              of transfer. Poly and unlimited poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(AbstractParent) :: Base(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
    end type

    type, extends(Base) :: Child(n3,k3)    ! (20,4,4,20,20,4)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)      j
    end type
end module

program transfer004
use m
    class(AbstractParent(:,4)), pointer :: c1(:,:,:)
    class(*), pointer :: b1(:)
    class(*), pointer :: v1(:)

    allocate(c1(2,2,2), SOURCE=reshape((/(Child(20,4,4,20,20,4)(i,-i),i=1,8)/), &
     (/2,2,2/)))
    allocate(b1(8), SOURCE=(/(Base(20,4,4,20)(i),i=101,108)/))
    allocate(v1(6), SOURCE=(/(Child(20,4,4,20,20,4)(i,-i),i=11,16)/))

    select type(name1=>transfer(pack(c1, MOD(c1%i,2)==0, v1), b1))
        type is (Base(*,4,4,*))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
