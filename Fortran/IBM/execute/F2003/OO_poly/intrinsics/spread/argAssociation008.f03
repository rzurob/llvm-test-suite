! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/20/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is a dummy argument. Dummy argument is a pointer or
!*  allocatable, non-poly, and is array.
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

program argAssociation008
use m
    type(Base), pointer :: b(:)
    type(Child), allocatable :: c(:,:)

    allocate(b(10), SOURCE=(/(Base(i),i=1,10)/))
    allocate(c(2,3), SOURCE=reshape((/(Child(i,-i),i=3,8)/), &
     (/2,3/), (/Child(-1,-2)/), (/2,1/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        type(Base), pointer :: arg1(:)
        type(Child), allocatable :: arg2(:,:)

        print *, spread(arg1, 2, 3)
        print *, spread(arg2, 3, 2)
    end subroutine
end
