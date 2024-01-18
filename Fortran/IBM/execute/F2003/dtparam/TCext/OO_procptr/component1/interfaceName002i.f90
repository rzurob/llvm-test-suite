! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodeferredlp /tstdev/OO_procptr/component1/interfaceName002i.f
! opt variations: -qnok -qnol -qdeferredlp

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
!                              interface-name, which is either a
!                              module subroutine or a module function
!                              Non-poly. Intrinsic or derived type,
!                              scalar or array.
!
!                              This test case use explicit interface
!                              implied by use association to declare the
!                              interface-name. The actual procedure
!                              associated has the same name as
!                              interface-name. The container data entity
!                              containing the procedure pointers is
!                              either pointer or allocatable.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type
end module

module m2
use m1
    contains

    function func1()
        integer :: func1(2,4)
        func1 = reshape((/(i,i=1,8)/), (/2,4/))
    end function

    function func2(b)
        type(Base(*,4)) :: b
        type(Base(20,4)) :: func2(3,5)
        func2 = reshape((/(Base(20,4)(b%i+j),j=1,15)/),(/3,5/))
    end function
end module

module m3
use m2
    type Container(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        procedure(func1), nopass, pointer :: pp1
        procedure(func2), nopass, pointer :: pp2
    end type
end module

program interfaceName002i
use m2
use m3
    type(Container(4,20)), pointer :: c1
    type(Container(4,20)), allocatable :: c2

    allocate(c1)
    c1%pp1 => func1
    c1%pp2 => func2

    print *, "func1", c1%pp1()
    print *, "func1", shape(c1%pp1())

    print *, "func2", c1%pp2(Base(20,4)(5))
    print *, "func2", shape(c1%pp2(Base(20,4)(5)))

    allocate(c2)
    c2 = c1

    print *, "func1", c2%pp1()
    print *, "func1", shape(c2%pp1())

    print *, "func2", c2%pp2(Base(20,4)(5))
    print *, "func2", shape(c2%pp2(Base(20,4)(5)))
end
