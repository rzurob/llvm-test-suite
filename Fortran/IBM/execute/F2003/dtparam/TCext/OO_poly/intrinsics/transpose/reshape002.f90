! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/transpose/reshape002.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/31/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Function return of transpose is the SOURCE of reshape.
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
        integer(k1) j
    end type
end module

program reshape002
use m
    type(Child(4)) :: c1(2,4)

    c1 = reshape((/(Child(4)(i,i-1),i=101,108)/), (/2,4/))

    print *, reshape(transpose(c1), (/3,2/))

    associate(name1=>reshape(transpose(c1), (/3,2/)))
        if(size(name1) .NE. 6) error stop 1_4
        if(name1(2,2)%i .NE. 102) error stop 2_4
    end associate
end
