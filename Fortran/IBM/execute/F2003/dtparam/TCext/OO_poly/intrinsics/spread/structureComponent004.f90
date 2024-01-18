! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=base /tstdev/OO_poly/intrinsics/spread/structureComponent004.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/18/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is a structure component, which is a scalar.
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
        type(Base(k1,n1)) :: b1
    end type
end module

program structureComponent004
use m
    type(Child(4,20)) :: c1(4,5)

    c1 = reshape((/(Child(4,20)(i,Base(4,20)(-i)),i=101,120)/), (/4,5/))

    associate(name1=>spread(c1%b1, 3, 2))
        if(.NOT. same_type_as(name1, Base(4,20)(1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end
