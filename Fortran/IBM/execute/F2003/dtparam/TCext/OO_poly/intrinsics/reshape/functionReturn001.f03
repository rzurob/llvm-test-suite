! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/reshape/functionReturn001.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/02/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is the return value of an
!*                               internal function call.
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type
end module

program functionReturn001
use m
    class(*), pointer :: b1(:,:) => null()

    print *, reshape(func1(), (/3,5/))
    allocate(b1(3,5), SOURCE=reshape(func2(), (/3,5/), &
     (/Base(20,4)(-1),Base(20,4)(-2)/), (/2,1/)))

    select type (b1)
        type is (Base(*,4))
            print *, b1
        class default
            error stop 1_4
    end select

    contains

    function func1()
        type(Base(20,4)) :: func1(20)
        func1 = (/ (Base(20,4)(i), i=1,20) /)
    end function

    function func2()
        class(Base(:,4)), pointer :: func2(:)
        allocate(func2(10), SOURCE=(/ (Base(20,4)(i), i=1,10) /))
    end function
end