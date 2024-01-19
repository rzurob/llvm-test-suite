! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/25/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE of RESHAPE is a dummy argument.
!*    Dummy argument is non-pointer, non-allocatable, and poly.
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

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        class(Base) :: arg1(:)
        class(Base) :: arg2(20)
        class(Base) :: arg3(10)
        class(Child) :: arg4(:)

        type(Base) :: x1(3,5)
        type(Child) :: y1(3,5)

        x1 = reshape(arg1, (/3,5/), (/Base(-1),Base(-2)/), (/2,1/))
        print *, x1
        x1 = reshape(arg2, (/3,5/))
        print *, x1
        x1 = reshape(arg3, (/3,5/), (/Base(-1),Base(-2)/), (/2,1/))
        print *, x1
        y1 = reshape(arg4, (/3,5/), (/Child(-1,1),Child(-2,2)/), (/2,1/))
        print *, y1
    end subroutine
end module

program argAssociation002
use m
    class(Base), allocatable :: ap1(:)
    class(Child), pointer :: b2(:) => null()
    type(Base) :: b1(10)
    type(Child) :: c1(20)

    allocate(ap1(10), SOURCE=(/ (Base(i), i=1,10) /))
    allocate(b2(20), SOURCE=(/ (Child(i,i+100), i=1,20) /))
    b1 = (/ (Base(i), i=-2,-20,-2) /)
    c1 = (/ (Child(i,i-1), i=31,50) /)

    call sub1(ap1, b2, b1, c1)
end
