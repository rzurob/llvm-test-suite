! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/declaration1/interfaceName004b.f
! opt variations: -qnol

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either an internal subroutine or
!                              an internal function. Non-poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case is diagnostic.
!                              The actual procedure associated has
!                              different name from interface-name.
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
end module

program interfaceName004b
use m
    interface
        subroutine interfaceSub1(i, b)
        import Base
            integer, intent(in) :: i
            type(Base(*,4)), intent(in) :: b
        end subroutine

        integer function interfaceFunc1()
        end function

        function interfaceFunc2(b)
        import Base
            type(Base(*,4)) :: b
            type(Base(20,4)) :: interfaceFunc2(5)
        end function
    end interface

    procedure(interfaceSub1), pointer :: pp1
    procedure(interfaceFunc1), pointer :: pp2
    procedure(interfaceFunc2), pointer :: pp3

    pp1 => sub1
    pp2 => func1
    pp3 => func2

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
        type(Base(20,4)) :: func2(5)
        func2 = b
    end function
end