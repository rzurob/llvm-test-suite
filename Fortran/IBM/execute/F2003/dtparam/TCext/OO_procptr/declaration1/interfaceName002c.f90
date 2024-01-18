! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/declaration1/interfaceName002c.f
! opt variations: -qnol

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either an external subroutine or
!                              an external function. Non-poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case use explicit interface to
!                              declare the interface-name before calling
!                              module subroutine and function. The
!                              actual procedure associated has different
!                              name from interface-name.
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

    real function func1()
        func1 = 20
    end function

    function func2(b)
        type(Base(*,4)) :: b
        type(Base(20,4)) :: func2
        func2 = b
    end function
end module

program interfaceName002c
use m
    interface
        subroutine interfaceSub1(i, b)
        use m
            integer, intent(in) :: i
            type(Base(*,4)), intent(in) :: b
        end subroutine

        real function interfaceFunc1()
        end function

        function interfaceFunc2(b)
        use m, only : Base
            type(Base(*,4)) :: b
            type(Base(20,4)) :: interfaceFunc2
        end function
    end interface

    procedure(interfaceSub1), pointer :: pp1
    procedure(interfaceFunc1), pointer :: pp2
    procedure(interfaceFunc2), pointer :: pp3

    pp1 => sub1
    pp2 => func1
    pp3 => func2

    call pp1(10, Base(20,4)(11))
    print *, "func1", int(pp2())
    print *, "func2", pp3(Base(20,4)(5))
end
