! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/reshape/reshape009.f
! opt variations: -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/20/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is non-poly
!*    Assigned data entity is unlimited poly
!*    PAD and ORDER are specified. PAD has different declared type but
!*      same dynamic type as SOURCE
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program reshape009
use m
    type(Child(4)) :: c1(10)
    class(*), allocatable :: b1(:,:)
    class(Base(4)), pointer :: b2(:)

    c1 = (/ (Child(4)(i,i+100), i=1,10) /)
    allocate(b2(2), SOURCE = (/Child(4)(-1,1),Child(4)(-2,2)/))

    allocate(b1(3,5), SOURCE = reshape(c1, (/3,5/), b2, (/2,1/)))

    print *, c1
    select type (b1)
        type is (Child(4))
            print *, b1
        class default
            error stop 1_4
    end select
end
