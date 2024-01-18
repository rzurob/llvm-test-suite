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
!                              which is either a module subroutine or
!                              a module function. Non-poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case use explicit interface
!                              implied by use association to declare the
!                              interface-name before calling module
!                              subroutine and function. The actual
!                              procedure associated has the same name as
!                              interface-name. The return value of
!                              function is array, and is either pointer
!                              or allocatable.
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

    function func1()
        integer, pointer :: func1(:,:)
        allocate(func1(2,4))
        func1 = reshape((/(i,i=1,8)/), (/2,4/))
    end function

    function func2(b)
        type(Base) :: b
        type(Base), allocatable :: func2(:,:)
        allocate(func2(3,5))
        func2 = reshape((/(Base(b%i+j),j=1,15)/), (/3,5/))
    end function
end module

program interfaceName002e
use m
    procedure(func1), pointer :: pp1
    procedure(func2), pointer :: pp2

    pp1 => func1
    pp2 => func2

    print *, "func1", pp1()
    print *, "func1", shape(pp1())

    print *, "func2", pp2(Base(5))
    print *, "func2", shape(pp2(Base(5)))
end
