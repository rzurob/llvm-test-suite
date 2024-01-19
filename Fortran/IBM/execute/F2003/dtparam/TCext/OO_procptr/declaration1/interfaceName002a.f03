! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/declaration1/interfaceName002a.f
! opt variations: -qnol

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either a module subroutine or
!                              a module function. Non-poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case use explicit interface
!                              implied by use association to declare the
!                              interface-name before calling module
!                              subroutine and function. The actual
!                              procedure associated has the same name as
!                              interface-name.
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

    subroutine sub1(i, b)
        integer, intent(in) :: i
        type(Base(*,4)), intent(in) :: b
        print *, "sub1", i, b
    end subroutine

    integer function func1()
        func1 = 20
    end function

    function func2(b)
        type(Base(*,4)) :: b
        type(Base(20,4)) :: func2(3)
        func2 = b
    end function
end module

program interfaceName002a
use m
    procedure(sub1), pointer :: pp1
    procedure(func1), pointer :: pp2
    procedure(func2), pointer :: pp3

    pp1 => sub1
    pp2 => func1
    pp3 => func2

    call pp1(10, Base(20,4)(11))
    print *, "func1", pp2()
    print *, "func2", pp3(Base(20,4)(5))
end
