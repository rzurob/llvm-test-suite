! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/declaration1/functionReturn002.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qnol

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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    contains

    function func1(i)
        integer, intent(in) :: i
        procedure(func2), pointer :: func1

        if(i .EQ. 2) then
            func1 => func2
        else
            func1 => func3
        endif
    end function

    function func2(b)
        type(Base(*,4)), intent(in) :: b
        type(Base(20,4)) :: func2
        func2 = Base(20,4)(b%i * 2)
    end function

    function func3(b)
        type(Base(*,4)), intent(in) :: b
        type(Base(20,4)) :: func3
        func3 = Base(20,4)(b%i * 3)
    end function
end module

program functionReturn002
use m
    procedure(func3), pointer :: pp1

    pp1 => func1(2)
    print *, "func2", pp1(Base(20,4)(5))

    pp1 => func1(3)
    print *, "func3", pp1(Base(20,4)(5))
end
