! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/pack/transfer003.f
! opt variations: -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/18/2005
! PRIMARY FUNCTIONS TESTED   : pack
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(AbstractParent) :: Base    ! (4)
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program transfer003
use m
    class(AbstractParent(4)), pointer :: c1(:,:,:)
    class(*), pointer :: b1(:)

    allocate(c1(2,2,2), SOURCE=reshape((/(Child(4)(i,i-1),i=101,108)/), &
     (/2,2,2/)))
    allocate(b1(12), SOURCE=(/(Base(4)(i),i=1,12)/))

    select type(name1=>pack(transfer(c1, b1), &
     reshape(MOD(c1%i,2)==0,(/16/),(/.FALSE./)), b1))
        type is (Base(4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>pack(transfer(b1, c1), &
     reshape(MOD(c1%i,2)==1,(/6/)), reshape(c1,(/8/))))
        type is (Child(4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
