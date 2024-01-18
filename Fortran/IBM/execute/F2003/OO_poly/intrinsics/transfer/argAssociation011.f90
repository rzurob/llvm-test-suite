! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/29/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD of TRANSFER is a dummy argument. Dummy argument
!*  is a pointer or allocatable, unlimited poly, and is scalar.
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
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    type Base1
        integer j
        type(Base) :: k
        integer m
    end type

    type, extends(Base1) :: Child1
        integer n
    end type
end module

program argAssociation011
use m
    class(*), pointer :: b
    class(*), allocatable :: b1

    allocate(b, SOURCE=Child(j=11,i=10))
    allocate(b1, SOURCE=Child1(6, Base(7), 8, 9))

    call sub1(b, b1)

    contains

    subroutine sub1(arg1, arg2)
        class(*), pointer :: arg1
        class(*), allocatable :: arg2

        select type(name1=>transfer(arg1, arg2))
            type is (Child1)
                print *, name1%j, name1%k%i
            class default
                error stop 1_4
        end select

        select type(name1=>transfer(arg2, arg1, 2))
            type is (Child)
                print *, name1
            class default
                error stop 2_4
        end select
    end subroutine
end
