! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/22/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE of RESHAPE is a dummy argument.
!*    Dummy argument is a pointer or allocatable, and unlimited poly.
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

program argAssociation006
use m
    class(*), allocatable :: b1(:,:)
    class(*), pointer :: c1(:,:)

    allocate(b1(4,5), SOURCE=reshape((/(Base(i), i=1,20)/),(/4,5/)))
    allocate(c1(2,5), SOURCE=reshape((/(Child(i,i-1), i=31,40)/), &
     (/2,5/)))

    call sub1(b1, c1)

    contains

    subroutine sub1(arg1, arg2)
        class(*), allocatable :: arg1(:,:)
        class(*), pointer :: arg2(:,:)

        select type (name1=>reshape(arg1, (/3,5/), &
         (/Base(-1),Base(-2)/), (/2,1/)))
            type is (Base)
                print *, name1
            class default
                error stop 1_4
        end select

        select type (name2=>reshape(arg2, (/3,2,3/), &
         (/Child(-1,1),Child(-2,2)/), (/3,2,1/)))
            type is (Child)
                print *, name2
            class default
                error stop 2_4
        end select
    end subroutine
end
