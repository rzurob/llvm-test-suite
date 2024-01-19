! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/transpose/structureComponent002.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/31/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    MATRIX is a structure component, which is poly array.
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

    type, extends(AbstractParent) :: Base(n2,k2,k3)    ! (4,20,20,4,4)
        integer, kind :: k2,k3
        integer, len  :: n2
        integer(k2)      i
        integer(k3)      j
    end type

    type, extends(Base) :: Child(k4)    ! (4,20,20,4,4,4)
        integer, kind                        :: k4
        class(AbstractParent(k4,:)), pointer :: b2(:,:)
    end type
end module

program structureComponent002
use m
    type(Base(4,20,20,4,4)) :: b1(20)
    type(Child(4,20,20,4,4,4)) :: c1

    b1 = (/ (Base(4,20,20,4,4)(i,i*2), i=1,20) /)

    allocate(c1%b2(4,6), SOURCE=reshape(b1, (/4,6/), &
     (/Base(4,20,20,4,4)(-1,-2),Base(4,20,20,4,4)(-3,-4)/), (/2,1/)))

    select type(name1=>transpose(c1%b2))
        type is (Base(4,*,*,4,4))
            print *, name1
            if(size(name1) .NE. 24) error stop 1_4
            if(ubound(name1, DIM=1) .NE. 6) error stop 2_4
            if(ubound(name1, DIM=2) .NE. 4) error stop 3_4
        class default
            error stop 4_4
    end select
end
