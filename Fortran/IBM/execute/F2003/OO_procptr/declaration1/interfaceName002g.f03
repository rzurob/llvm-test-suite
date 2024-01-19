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
!                              interface-name. The dummy arguments of
!                              the associated procedure are arrays. This
!                              test case involves sequence association
!                              where the size and shape of the actual
!                              argument do not match those of the
!                              dummy argument.
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

    subroutine sub1(i)
        integer, intent(in) :: i(2,4)
        print *, "sub1", i
    end subroutine

    integer function func1(b)
        integer :: b(3:*)
        print *, "func1", b(:8)
        func1 = size(b(:8))
    end function

    integer function func2(b)
        type(Base) :: b(5,3)
        print *, "func2", b
        func2 = size(b)
    end function
end module

program interfaceName002g
use m
    procedure(sub1), pointer :: pp1
    procedure(func1), pointer :: pp2
    procedure(func2), pointer :: pp3

    integer rv
    integer :: i1(3,2)
    i1 = reshape((/(i,i=1,6)/),(/3,2/),(/-1/),(/2,1/))

    pp1 => sub1
    pp2 => func1
    pp3 => func2

    call pp1(reshape((/(i,i=1,18)/),(/3,3,3/),(/-1,-2/),(/2,3,1/)))

    rv = pp2(i1)
    print *, "Func1", rv

    rv = pp3(reshape((/(Base(i),i=11,25)/),(/4,3,2/), &
     (/Base(-1),Base(-2)/), (/2,3,1/)))
    print *, "Func2", rv
end
