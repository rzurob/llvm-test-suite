! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/transfer/structureComponent002.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/21/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD is a structure component, which is poly
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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
        integer(k1) j
    end type

    type, extends(Base) :: Child    ! (4,20)
        class(Base(k1,:)), pointer :: b2(:,:)
    end type

    type Base1(n2,k2)    ! (20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)   :: j(2,2)
    end type
end module

program structureComponent002
use m
    type(Base(4,20)) :: b1(20)
    type(Child(4,20)) :: c1
    type(Base1(20,4)) :: b2

    b1 = (/ (Base(4,20)(i,i*2), i=1,20) /)

    allocate(c1%b2(5,5), SOURCE=reshape(b1, (/5,5/), &
     (/Base(4,20)(-1,-2),Base(4,20)(-3,-4)/), (/2,1/)))

    associate(name1=>transfer(c1%b2, b2%j, 20))
        print *, name1
        if(size(name1) .NE. 20) error stop 1_4
    end associate

    associate(name1=>transfer(c1%b2, b2%j))
        print *, name1
        if(size(name1) .NE. 50) error stop 2_4
    end associate
end
