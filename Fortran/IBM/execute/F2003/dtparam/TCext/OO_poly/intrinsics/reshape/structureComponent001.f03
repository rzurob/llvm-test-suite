! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/reshape/structureComponent001.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/06/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is a structure component, which
!*    is non-poly array. The object containing the component is a scalar.
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

    type, extends(AbstractParent) :: Base(n2,k2)    ! (4,20,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
    end type

    type, extends(Base) :: Child(k3,n3)    ! (4,20,20,4,4,20)
        integer, kind           :: k3
        integer, len            :: n3
        type(Base(k3,n3,n3,k3)) :: b1(20)
        type(Base(k3,n3,n3,k3)) :: b2(5,5)
    end type
end module

program structureComponent001
use m
    class(Base(4,:,:,4)), allocatable :: b0(:,:,:)
    type(Child(4,20,20,4,4,20)) :: c1

    c1%b1 = (/ (Base(4,20,20,4)(i), i=1,20) /)

    c1%b2 = reshape(c1%b1, (/5,5/), (/Base(4,20,20,4)(-1),Base(4,20,20,4)(-2)/), (/2,1/))

    allocate(b0(3,2,4), SOURCE=reshape(c1%b2, (/3,2,4/), &
     (/Base(4,20,20,4)(-3)/), (/3,2,1/)))

    print *, c1%b1
    print *, c1%b2

    select type (b0)
        type is (Base(4,*,*,4))
            print *, b0
        class default
            error stop 1_4
    end select
end
