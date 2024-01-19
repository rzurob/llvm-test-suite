!=======================================================================
! TEST BUCKET                : OO_procptr/component
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Declare a procedure pointer component
!                              using procedure declaration statement.
!                              specify procedure interface using
!                              interface-name, which is either an
!                              external subroutine or an external
!                              function. Non-poly. Intrinsic or derived
!                              type, scalar or array.
!
!                              This test case use explicit interface to
!                              declare the interface-name before calling
!                              external subroutine and function. The
!                              actual procedure associated has the same
!                              name as interface-name. The return value
!                              of the associated function is array and
!                              is either allocatable or pointer. The
!                              dummy arguments are also arrays.
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

    type Child
        integer i
        integer j
    end type
end module

module n
use m
    interface
        subroutine sub1(b, c)
        use m
            type(Base), intent(in) :: b(5)
            type(Child), intent(in) :: c(2,4)
        end subroutine

        function func1(i)
            integer, intent(in) :: i(8)
            integer, allocatable :: func1(:,:)
        end function

        function func2(b)
        use m
            type(Base) :: b(3,5)
            type(Child), pointer :: func2(:)
        end function
    end interface

    type Container
        procedure(sub1), nopass, pointer :: pp1 => null()
        procedure(func1), nopass, pointer :: pp2 => null()
        procedure(func2), nopass, pointer :: pp3 => null()
    end type
end module

program interfaceName001e
use n
    implicit type(Container) (c)

    c1%pp1 => sub1
    c1%pp2 => func1
    c1%pp3 => func2

    call c1%pp1((/(Base(i),i=1,5)/), reshape((/(Child(i,-i), &
     i=11,18)/), (/2,4/)))
    print *, "func1", c1%pp2((/(i,i=101,108)/))
    print *, "func1", shape(c1%pp2((/(i,i=101,108)/)))
    print *, "func2", c1%pp3(reshape((/(Base(-i),i=1,15)/), (/3,5/)))
    print *, "func2", shape(c1%pp3(reshape((/(Base(-i),i=1,15)/), &
     (/3,5/))))
end

subroutine sub1(b, c)
use m
    type(Base), intent(in) :: b(5)
    type(Child), intent(in) :: c(2,4)
    print *, "sub1", b
    print *, "sub1", shape(b)
    print *, "sub1", c
    print *, "sub1", shape(c)
end subroutine

function func1(i)
    integer, intent(in) :: i(8)
    integer, allocatable :: func1(:,:)
    allocate(func1(4,3))
    func1 = reshape(i,(/4,3/),(/-1/),(/2,1/))
end function

function func2(b)
use m
    type(Base) :: b(3,5)
    type(Child), pointer :: func2(:)
    allocate(func2(7))
    func2 = transfer(b,Child(1,1),7)
end function
