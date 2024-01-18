! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/intrinsics/transfer/structureComponent001.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/21/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD is a structure component, which is non-poly
!*  array. The object containing the component is a scalar.
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4)
        type(Base(k1)) :: b1(20)
        type(Base(k1)) :: b2(5,5)
    end type

    type Base1(k2)    ! (4)
        integer, kind  :: k2
        integer(k2)       i
        type(Base(k2)) :: j(2,2)
    end type
end module

program structureComponent001
use m
    type(Child(4)) :: c1
    type(Base1(4)) :: b1

    c1%b1 = (/ (Base(4)(i), i=1,20) /)

    c1%b2 = reshape(c1%b1, (/5,5/), (/Base(4)(-1),Base(4)(-2)/), (/2,1/))

    associate(name1=>transfer(c1%b2, b1%j))
        print *, c1%b2
        print *, name1
        if(size(name1) .NE. 25) error stop 1_4
    end associate
end
