! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/31/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Function return of transpose is the SOURCE of reshape. Poly and
!*  unlimited poly.
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
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program reshape004
use m
    class(AbstractParent), pointer :: c1(:)
    class(*), pointer :: b1(:)

    allocate(c1(5), SOURCE=(/(Child(i,i-1),i=101,105)/))
    allocate(b1(30), SOURCE=(/(Base(i),i=1,30)/))

    select type(name1=>reshape(transpose(reshape(c1, (/2,4/), &
     (/Child(-1,-2)/), (/2,1/))), (/2,3/)))
        type is (Child)
            print *, name1
            if(size(name1) .NE. 6) error stop 1_4
            if(ubound(name1, DIM=1) .NE. 2) error stop 2_4
            if(ubound(name1, DIM=2) .NE. 3) error stop 3_4
        class default
            error stop 4_4
    end select

    select type(name1=>reshape(transpose(reshape(b1,(/5,3/))),(/4,2/)))
        type is (Base)
            print *, name1
            if(size(name1) .NE. 8) error stop 5_4
            if(ubound(name1, DIM=1) .NE. 4) error stop 6_4
            if(ubound(name1, DIM=2) .NE. 2) error stop 7_4
        class default
            error stop 8_4
    end select
end
