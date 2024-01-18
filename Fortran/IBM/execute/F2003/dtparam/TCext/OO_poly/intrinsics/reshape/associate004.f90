! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/reshape/associate004.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/25/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is an associate name. Selector
!*    is array section and is a structure component.
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
        integer(k1) j
        type(Base(k1,n1)) :: b(5,6)
    end type
end module

program associate004
use m
    class(AbstractParent(4,:)), pointer :: ap1(:,:,:) => null()
    type(Child(4,20)) :: c1
    c1%i = 8
    c1%j = 9
    c1%b = reshape((/ (Base(4,20)(i), i=1,30) /), (/5,6/))

    associate(name1=>c1%b(1:5:2,2:6:2))
        allocate(ap1(2,3,2), SOURCE= reshape(name1, &
         (/2,3,2/), (/Base(4,20)(-1),Base(4,20)(-2)/), (/1,2,3/)))
    end associate

    select type (ap1)
        type is (Base(4,*))
            print *, ap1
        class default
            error stop 1_4
    end select
end
