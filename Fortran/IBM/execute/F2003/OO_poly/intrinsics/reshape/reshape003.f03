! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/20/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is non-poly
!*    Assigned data entity is non-poly
!*    PAD and ORDER are specified, PAD has different declared type
!*      but same dynamic type as SOURCE
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
        integer :: i = 88
    end type

    type, extends(Base) :: Child
        integer :: j = 99
    end type
end module

program reshape003
use m
    type(Child) :: c1(10)
    type(Child) :: c2(3,5)
    class(Base), allocatable :: b1(:)
    c1 = (/ (Child(i,i+100), i=1,10) /)
    allocate(Child::b1(2))

    c2 = reshape(c1, (/3,5/), b1, (/2,1/))

    print *, c1
    print *, c2
end