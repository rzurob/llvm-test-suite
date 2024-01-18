! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/component1/interfaceName003h.f
! opt variations: -qnok -ql

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Specify procedure interface using
!                              interface-name. The procedure pointer
!                              is a dummy argument, and the associated
!                              procedure is given using a procedure
!                              pointer component. Non-poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case use explicit interface
!                              and interface implied by use association
!                              to declare interface-name. The actual
!                              procedure associated has different name
!                              from interface-name. The return value of
!                              function is array, and is either pointer
!                              or allocatable. The dummy argument of
!                              function is array, and is either pointer
!                              or allocatable.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    interface
        function interfaceFunc1(b)
        import Base
            type(Base(4)), allocatable, intent(in) :: b(:)
            type(Base(4)), pointer :: interfaceFunc1(:,:)
        end function

        function interfaceFunc2(b, p)
        import Base, interfaceFunc1
            type(Base(4)), allocatable, intent(in) :: b(:)
            procedure(interfaceFunc1), pointer, intent(in) :: p
            type(Base(4)), pointer :: interfaceFunc2(:,:)
        end function
    end interface
end module

module m2
use m1
    contains

    function func1(b)
        type(Base(4)), allocatable, intent(in) :: b(:)
        type(Base(4)), pointer :: func1(:,:)
        allocate(func1(3,5))
        func1 = reshape(b,(/3,5/), (/Base(4)(-1),Base(4)(-2)/),(/2,1/))
    end function

    function func2(b, p)
        type(Base(4)), allocatable, intent(in) :: b(:)
        ! interface-name is explicit by use association
        procedure(interfaceFunc1), pointer, intent(in) :: p
        type(Base(4)), pointer :: func2(:,:)
        allocate(func2(3,5))
        func2 = p(b)
    end function
end module

program interfaceName003h
use m2
    type Container(k2)    ! (4)
        integer, kind :: k2
        procedure(interfaceFunc1), nopass, pointer :: pp1
        procedure(interfaceFunc2), nopass, pointer :: pp2
    end type

    type(Base(4)), allocatable :: b1(:)

    type(Container(4)) :: c1
    c1%pp1 => func1
    c1%pp2 => func2

    allocate(b1(10))
    b1 = (/(Base(4)(i),i=1,10)/)

    print *, c1%pp2(b1, c1%pp1)
    print *, shape(c1%pp2(b1, c1%pp1))
end
