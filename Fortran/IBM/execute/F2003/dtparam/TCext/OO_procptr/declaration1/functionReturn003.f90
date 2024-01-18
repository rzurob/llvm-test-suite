! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/declaration1/functionReturn003.f
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    contains

    function func0()
        procedure(type(Base(20,4))), pointer :: func0
        func0 => func2
    end function

    function func1()
        procedure(func3), pointer :: func1
        func1 => func3
    end function

    function func2(i)
        integer, intent(in) :: i
        type(Base(20,4)) :: func2
        func2 = Base(20,4)(i * 2)
    end function

    function func3(b)
        type(Base(*,4)), intent(in) :: b
        type(Base(20,4)) :: func3
        func3 = Base(20,4)(b%i + 2)
    end function
end module

program functionReturn003
use m
    implicit type(Base(20,4)) (p)

    procedure(), pointer :: pp1 => null()
    procedure(func3), pointer :: pp2 => null()
    if(associated(pp1)) error stop 1_4

    pp1 => func0()
    if(.NOT. associated(pp1)) error stop 2_4
    print *, "func2", pp1(5)

    pp1 => null()
    if(associated(pp1)) error stop 3_4

    pp2 => func1()
    if(.NOT. associated(pp2)) error stop 4_4
    print *, "func3", pp2(Base(20,4)(5))
end
