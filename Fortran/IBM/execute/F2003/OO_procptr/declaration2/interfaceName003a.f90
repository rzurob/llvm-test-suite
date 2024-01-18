!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Specify procedure interface using
!                              interface-name. The procedure pointer
!                              is a dummy argument. Poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case use explicit interface
!                              implied by use association to declare
!                              interface-name. The actual procedure
!                              associated has different name from
!                              interface-name. The return value of
!                              function is array.
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

    type, extends(Base) :: Child
        integer j
    end type

    interface
        function interfaceFunc1(b)
        import Base
            class(*), pointer, intent(in) :: b(:)
            type(Base) :: interfaceFunc1(3,5)
        end function

        function interfaceFunc2(b, p)
        import Base, interfaceFunc1
            class(*), pointer, intent(in) :: b(:)
            procedure(interfaceFunc1), pointer, intent(in) :: p
            type(Base) :: interfaceFunc2(3,5)
        end function
    end interface

    contains

    function func1(b)
        class(*), pointer, intent(in) :: b(:)
        type(Base) :: func1(3,5)
        select type (b)
            type is (Base)
                func1 = reshape(b,(/3,5/),(/Base(-1),Base(-2)/),(/2,1/))
            class default
                error stop 1_4
        end select
    end function

    function func2(b, p)
        class(*), pointer, intent(in) :: b(:)
        procedure(interfaceFunc1), pointer, intent(in) :: p
        type(Base) :: func2(3,5)
        func2 = p(b)
    end function
end module

program interfaceName003a
use m
    procedure(interfaceFunc1), pointer :: pp1
    procedure(interfaceFunc2), pointer :: pp2
    class(*), pointer :: b1(:)

    pp1 => func1
    pp2 => func2

    allocate(b1(10), SOURCE=(/(Base(i),i=1,10)/))
    print *, pp2(b1, pp1)
    print *, shape(pp2(b1, pp1))
end
