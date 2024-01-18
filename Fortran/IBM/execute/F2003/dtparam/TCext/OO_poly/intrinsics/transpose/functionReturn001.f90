! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/transpose/functionReturn001.f
! opt variations: -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/31/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    MATRIX is the return value of an internal function call.
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program functionReturn001
use m
    class(*), pointer :: b1(:,:) => null()

    print *, transpose(func1())

    select type(name1=>transpose(func2()))
        type is (Child(4))
            print *, name1
            if(size(name1) .NE. 8) error stop 1_4
            if(ubound(name1, DIM=1) .NE. 2) error stop 2_4
            if(ubound(name1, DIM=2) .NE. 4) error stop 3_4
        class default
            error stop 4_4
    end select

    contains

    function func1()
        type(Base(4)) :: func1(3,4)
        func1 = reshape((/(Base(4)(i), i=1,20)/), (/3,4/))
    end function

    function func2()
        class(Base(4)), pointer :: func2(:,:)
        allocate(func2(4,2), SOURCE=reshape((/(Child(4)(i,i+1),i=1,9)/), &
         (/4,2/)))
    end function
end
