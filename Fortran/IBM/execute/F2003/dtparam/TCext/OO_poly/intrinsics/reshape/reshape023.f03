! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/reshape/reshape023.f
! opt variations: -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/05/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is non-poly
!*    Assigned data entity is poly
!*    PAD and ORDER are not specified
!*    SOURCE is rank three
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

program reshape023
use m
    type(Child(4)) :: c1(20)
    type(Child(4)) :: c2(3,4,2)
    class(Base(4)), pointer :: b2(:,:) => null()

    c1 = (/ (Child(4)(i,i+1), i=1,20) /)
    c2 = reshape(c1, (/3,4,2/), (/Child(4)(-1,1),Child(4)(-2,2)/), (/2,3,1/))

    allocate(b2(3,5), SOURCE=reshape(c2, (/3,5/)))

    print *, c1
    print *, c2

    select type (b2)
        type is (Child(4))
            print *, b2
        class default
            error stop 1_4
    end select
end
