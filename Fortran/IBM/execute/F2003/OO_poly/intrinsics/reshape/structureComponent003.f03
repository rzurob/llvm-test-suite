! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/06/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is a structure component, which
!*    is unlimited poly array. The object containing the component is a
!*    scalar.
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
        class(*), allocatable :: b2(:,:)
    end type
end module

program structureComponent003
use m
    class(*), allocatable :: b0(:,:,:)
    type(Base) :: b1(20)
    type(Child) :: c1

    b1 = (/ (Base(i), i=1,20) /)

    allocate(c1%b2(5,5), SOURCE=reshape(b1, (/5,5/), &
     (/Base(-1),Base(-2)/), (/2,1/)))

    allocate(b0(3,2,4), SOURCE=reshape(c1%b2, (/3,2,4/), &
     (/Base(-3)/), (/3,2,1/)))

    print *, b1

    select type (b0)
        type is (Base)
            print *, b0
        class default
            error stop 1_4
    end select
end
