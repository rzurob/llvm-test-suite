!***********************************************************************
!* =====================================================================
!* DATE                       : 12/30/2004
!* PRIMARY FUNCTIONS TESTED   : transfer
!* SECONDARY FUNCTIONS TESTED :
!* DESCRIPTION                :
!*   SOURCE is the return value of an internal function call.
!* KEYWORD(S)                 :
!* TARGET(S)                  :
!* =====================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 03/29/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Removed the TRUN header.
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

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
end module

program functionReturn001
use m
    class(*), pointer :: b1(:,:) => null()

    print *, transfer(func1(), Child(1,1), 4)
    associate(name1=>transfer(func2(), (/Base1(1,Base(1),1)/)))
        if(size(name1) .NE. 6) error stop 1_4
        print *, name1
    end associate

    contains

    function func1()
        type(Base) :: func1(20)
        func1 = (/ (Base(i), i=1,20) /)
    end function

    function func2()
        class(Base), pointer :: func2(:)
        allocate(func2(9), SOURCE=(/ (Child(i,i+1), i=1,9) /))
    end function
end
