! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/declaration1/interfaceName001b.f
! opt variations: -qnol

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either an external subroutine or
!                              an external function. Non-poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case use explicit interface
!                              implied by use association to call
!                              external procedures. It also involves
!                              implicit typing. The actual procedure
!                              name has the same name as interface-name.
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

    interface
        subroutine sub1(i, b)
        import Base
            integer, intent(in) :: i
            type(Base(*,4)), intent(in) :: b
        end subroutine

        integer function ifunc1()
        end function

        real function func2(b)
        import Base
            type(Base(*,4)) :: b
        end function
    end interface
end module

program interfaceName001b
use m
    implicit real (i), type(Base(20,4)) (f)
    procedure(sub1), pointer :: pp1
    procedure(ifunc1), pointer :: pp2
    procedure(func2), pointer :: pp3

    pp1 => sub1
    pp2 => ifunc1
    pp3 => func2

    call pp1(10, Base(20,4)(11))
    print *, "ifunc1", pp2()
    print *, "func2", int(pp3(Base(20,4)(5)))
end

subroutine sub1(i, b)
use m, only : Base
    integer, intent(in) :: i
    type(Base(*,4)), intent(in) :: b
    print *, "sub1", i, b
end subroutine

integer function ifunc1()
    ifunc1 = 20
end function

real function func2(b)
use m, only : Base
    type(Base(*,4)) :: b
    func2 = b%i
end function
