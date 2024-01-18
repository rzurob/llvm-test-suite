! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=none /tstdev/OO_poly/intrinsics/cshift/transfer003.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 01/31/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : ARRAY is function return of transfer.
!                              Poly and unlimited poly.
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

    type, extends(AbstractParent) :: Base(k2)    ! (4,20,4)
        integer, kind :: k2
        integer(k2)      i
    end type

    type, extends(Base) :: Child(k3)    ! (4,20,4,4)
        integer, kind :: k3
        integer(k3)      j
    end type
end module

program transfer003
use m
    class(AbstractParent(4,20)), pointer :: c1(:,:,:)
    class(*), pointer :: b1(:)

    allocate(c1(2,2,2), SOURCE=reshape((/(Child(4,20,4,4)(i,i-1),i=101,108)/), &
     (/2,2,2/)))
    allocate(b1(12), SOURCE=(/(Base(4,20,4)(i),i=1,12)/))

    select type(name1=>cshift(transfer(c1, b1), 11))
        type is (Base(4,*,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>cshift(transfer(b1, c1), -4, 1))
        type is (Child(4,*,4,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
