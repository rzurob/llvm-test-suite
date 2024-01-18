! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=base /tstdev/OO_poly/intrinsics/transfer/structureComponent004.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/21/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD is a structure component, which is a scalar.
!*  The object containing the component is an array.
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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        type(Base(k1,n1)) :: b2
    end type
end module

program structureComponent004
use m
    type(Child(4,20)) :: c1(4,5)

    c1%i = reshape((/(i,i=101,120)/), (/4,5/))
    c1%b2 = reshape((/(Base(4,20)(i),i=1,20)/),(/4,5/))

    print *, transfer(c1%b2, c1)
    if(size(transfer(c1%b2, c1)) .NE. 10) error stop 1_4
    associate(name1=>transfer(c1%b2, c1))
        print *, name1
        if(size(name1) .NE. 10) error stop 2_4
    end associate
end
