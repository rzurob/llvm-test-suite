! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/OO_procptr/declaration1/interfaceName002e.f
! opt variations: -qnol -qdeferredlp

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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    contains

    function func1()
        integer, pointer :: func1(:,:)
        allocate(func1(2,4))
        func1 = reshape((/(i,i=1,8)/), (/2,4/))
    end function

    function func2(b)
        type(Base(*,4)) :: b
        type(Base(20,4)), allocatable :: func2(:,:)
        allocate(func2(3,5))
        func2 = reshape((/(Base(20,4)(b%i+j),j=1,15)/), (/3,5/))
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

    print *, "func2", pp2(Base(20,4)(5))
    print *, "func2", shape(pp2(Base(20,4)(5)))
end