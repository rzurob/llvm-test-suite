! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/intrinsics/transpose/structureComponent004.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/31/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    MATRIX is a structure component, which is a scalar. The object
!*  containing the component is an array.
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
        type(Base(k1)) :: b2
    end type
end module

program structureComponent004
use m
    type(Child(4)) :: c1(4,5)

    c1%b2 = reshape((/(Base(4)(i),i=1,20)/),(/4,5/))

    print *, transpose(c1%b2)
    if(size(transpose(c1%b2)) .NE. 20) error stop 1_4
    associate(name1=>transpose(c1%b2))
        print *, name1
        if(size(name1) .NE. 20) error stop 2_4
        if(ubound(name1, DIM=1) .NE. 5) error stop 3_4
        if(ubound(name1, DIM=2) .NE. 4) error stop 4_4
    end associate
end