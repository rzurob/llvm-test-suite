! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/20/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is a dummy argument. Dummy argument is non-pointer,
!*  non-allocatable, non-poly, and is scalar.
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

program argAssociation001
use m
    type(Base) :: b
    class(Base), pointer :: c

    b = Base(10)
    allocate(c, SOURCE=Child(3,4))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        type(Base) :: arg1
        type(Base) :: arg2

        print *, spread(arg1, 1, 3)
        print *, size(spread(arg1, 1, 3))
        print *, shape(spread(arg1, 1, 3))

        print *, spread(arg2, 1, 5)
        print *, size(spread(arg2, 1, 5))
        print *, shape(spread(arg2, 1, 5))
    end subroutine
end
