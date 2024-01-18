! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/component1/interfaceName003e.f
! opt variations: -qnok -qnol

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
!                              function is array.
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

    interface
        function interfaceFunc1(b)
        import Base
            type(Base(*,4)), intent(in) :: b(10)
            type(Base(20,4)) :: interfaceFunc1(3,5)
        end function

        function interfaceFunc2(b, p)
        import Base, interfaceFunc1
            type(Base(*,4)), intent(in) :: b(10)
            procedure(interfaceFunc1), pointer, intent(in) :: p
            type(Base(20,4)) :: interfaceFunc2(3,5)
        end function
    end interface
end module

module m2
use m1
    contains

    function func1(b)
        type(Base(*,4)), intent(in) :: b(10)
        type(Base(20,4)) :: func1(3,5)
        func1 = reshape(b,(/3,5/),(/Base(20,4)(-1),Base(20,4)(-2)/),(/2,1/))
    end function

    function func2(b, p)
        type(Base(*,4)), intent(in) :: b(10)
        ! interface-name is explicit by use association
        procedure(interfaceFunc1), pointer, intent(in) :: p
        type(Base(20,4)) :: func2(3,5)
        func2 = p(b)
    end function
end module

program interfaceName003e
use m2
    type Container(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        procedure(interfaceFunc1), nopass, pointer :: pp1
        procedure(interfaceFunc2), nopass, pointer :: pp2
    end type

    type(Container(4,20)) :: c1
    c1%pp1 => func1
    c1%pp2 => func2

    print *, c1%pp2((/(Base(20,4)(i),i=1,10)/), c1%pp1)
    print *, shape(c1%pp2((/(Base(20,4)(i),i=1,10)/), c1%pp1))
end
