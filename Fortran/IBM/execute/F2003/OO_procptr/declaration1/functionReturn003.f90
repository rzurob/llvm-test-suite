!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Do not
!                              specify proc-interface.
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

        if(i .EQ. 1) then
            func1 => func2
        else
            func1 => func3
        endif
    end function

    function func2(i)
        integer, intent(in) :: i
        type(Base) :: func2
        func2 = Base(i * 2)
    end function

    function func3(b)
        type(Base), intent(in) :: b
        type(Base) :: func3
        func3 = Base(b%i + 2)
    end function
end module

program functionReturn003
use m
    implicit type(Base) (p)

    procedure(), pointer :: pp1 => null()
    if(associated(pp1)) error stop 1_4

    pp1 => func1(1)
    if(.NOT. associated(pp1)) error stop 2_4
    print *, "func2", pp1(5)

    pp1 => null()
    if(associated(pp1)) error stop 3_4

    pp1 => func1(2)
    if(.NOT. associated(pp1)) error stop 4_4
    print *, "func3", pp1(Base(5))
end
