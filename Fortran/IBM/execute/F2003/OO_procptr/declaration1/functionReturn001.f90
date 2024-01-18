!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Specify
!                              proc-interface using interface-name.
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

    subroutine sub1(b)
        type(Base), intent(in) :: b
        print *, "sub1", b
    end subroutine

    subroutine sub2(b)
        type(Base), intent(in) :: b
        print *, "sub2", b
    end subroutine

    function func1(i)
        integer, intent(in) :: i
        procedure(sub1), pointer :: func1

        if(i .EQ. 1) then
            func1 => sub1
        else
            func1 => sub2
        endif
    end function
end module

program functionReturn001
use m
    procedure(sub1), pointer :: pp1

    pp1 => func1(1)
    call pp1(Base(5))

    pp1 => func1(2)
    call pp1(Base(5))
end
