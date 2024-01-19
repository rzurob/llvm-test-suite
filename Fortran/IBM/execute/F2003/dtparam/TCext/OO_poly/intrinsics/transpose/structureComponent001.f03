! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=base /tstdev/OO_poly/intrinsics/transpose/structureComponent001.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/31/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    MATRIX is a structure component, which is non-poly array.
!*  The object containing the component is a scalar.
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
        type(Base(k1,n1)) :: b1(4,6)
    end type
end module

program structureComponent001
use m
    type(Child(4,20)) :: c1

    c1%b1 = reshape((/(Base(4,20)(i), i=1,20)/), (/4,6/), &
     (/Base(4,20)(-1),Base(4,20)(-2)/), (/2,1/))

    associate(name1=>transpose(c1%b1))
        print *, name1
        if(size(name1) .NE. 24) error stop 1_4
        if(ubound(name1, DIM=1) .NE. 6) error stop 2_4
        if(ubound(name1, DIM=2) .NE. 4) error stop 3_4
    end associate
end
