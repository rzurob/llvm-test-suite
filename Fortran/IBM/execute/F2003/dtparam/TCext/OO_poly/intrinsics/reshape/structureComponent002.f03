! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/reshape/structureComponent002.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/06/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is a structure component, which
!*    is poly array. The object containing the component is a scalar.
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
        class(Base(k1,:)), allocatable :: b2(:,:)
    end type
end module

program structureComponent002
use m
    class(Base(4,:)), allocatable :: b0(:,:,:)
    type(Base(4,20)) :: b1(20)
    type(Child(4,20)) :: c1

    b1 = (/ (Base(4,20)(i), i=1,20) /)

    allocate(c1%b2(5,5), SOURCE=reshape(b1, (/5,5/), &
     (/Base(4,20)(-1),Base(4,20)(-2)/), (/2,1/)))

    allocate(b0(3,2,4), SOURCE=reshape(c1%b2, (/3,2,4/), &
     (/Base(4,20)(-3)/), (/3,2,1/)))

    print *, b1

    select type (b0)
        type is (Base(4,*))
            print *, b0
        class default
            error stop 1_4
    end select
end