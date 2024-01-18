!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Declare a procedure pointer component
!                              using procedure declaration statement.
!                              specify procedure interface using
!                              interface-name, which is either an
!                              external subroutine or an external
!                              function. Non-poly. Intrinsic or derived
!                              type, scalar.
!
!                              This test case use explicit interface
!                              implied by use association to declare
!                              interface-name. It also involves
!                              implicit typing. The actual procedure
!                              name has the same name as interface-name.
!                              Also use the NULL() intrinsic to
!                              initialize procedure pointer component
!                              in structure constructor.
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
end module

module n
use m
    interface
        subroutine sub1(i, b)
        import Base
            integer, intent(in) :: i
            type(Base), intent(in) :: b
        end subroutine

        integer function ifunc1()
        end function

        real function func2(b)
        use m
            type(Base) :: b
        end function
    end interface
end module

program interfaceName001b
use n
    implicit real (i), type(Base) (f)

    type Container
        integer j
        procedure(sub1), nopass, pointer :: pp1
        procedure(ifunc1), nopass, pointer :: pp2
        procedure(func2), nopass, pointer :: pp3
    end type

    type(Container) :: b1

    b1 = Container(11, null(), null(), null())
    b1%pp1 => sub1
    b1%pp2 => ifunc1
    b1%pp3 => func2

    call b1%pp1(10, Base(b1%j))
    print *, "ifunc1", b1%pp2()
    b1%j = int(b1%pp3(Base(5)))
    print *, "func2", b1%j
end

subroutine sub1(i, b)
use m, only : Base
    integer, intent(in) :: i
    type(Base), intent(in) :: b
    print *, "sub1", i, b
end subroutine

integer function ifunc1()
    ifunc1 = 20
end function

real function func2(b)
use m, only : Base
    type(Base) :: b
    func2 = b%i
end function
