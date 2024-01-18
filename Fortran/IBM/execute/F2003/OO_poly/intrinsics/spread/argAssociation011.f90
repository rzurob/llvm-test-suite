! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/20/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is a dummy argument. Dummy argument is a pointer or
!*  allocatable, unlimited poly, and is scalar.
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
end module

program argAssociation011
use m
    class(*), pointer :: b
    class(*), allocatable :: c

    allocate(b, SOURCE=Child(j=11,i=10))
    allocate(c, SOURCE=Base(6))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        class(*), pointer :: arg1
        class(*), allocatable :: arg2

        select type(name1=>spread(arg1, 1, 5))
            type is (Child)
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select

        select type(name1=>spread(arg2, 1, 10))
            type is (Base)
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 2_4
        end select
    end subroutine
end
