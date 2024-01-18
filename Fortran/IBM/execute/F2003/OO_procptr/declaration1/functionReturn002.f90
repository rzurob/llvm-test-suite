!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Specify
!                              proc-interface using declaration type
!                              specification.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type

    contains

    function func1(i)
        integer, intent(in) :: i
        procedure(type(Base)), pointer :: func1

        if(i .EQ. 2) then
            func1 => func2
        else
            func1 => func3
        endif
    end function

    function func2(b)
        type(Base), intent(in) :: b
        type(Base) :: func2
        func2 = Base(b%i * 2)
    end function

    function func3(b)
        type(Base), intent(in) :: b
        type(Base) :: func3
        func3 = Base(b%i * 3)
    end function
end module

program functionReturn002
use m
    procedure(type(Base)), pointer :: pp1

    pp1 => func1(2)
    print *, "func2", pp1(Base(5))

    pp1 => func1(3)
    print *, "func3", pp1(Base(5))
end
