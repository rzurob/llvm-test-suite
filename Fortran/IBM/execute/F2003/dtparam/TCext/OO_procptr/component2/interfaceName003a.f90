! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_procptr/component2/interfaceName003a.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Specify procedure interface using
!                              interface-name. The procedure pointer is
!                              a dummy argument. Unlimited poly, scalar
!                              or array.
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    interface
        function interfaceFunc1(b)
        import Base
            class(*), pointer, intent(in) :: b(:)
            type(Base(20,4)) :: interfaceFunc1(3,5)
        end function

        function interfaceFunc2(b, p)
        import Base, interfaceFunc1
            class(*), pointer, intent(in) :: b(:)
            procedure(interfaceFunc1), pointer, intent(in) :: p
            type(Base(20,4)) :: interfaceFunc2(3,5)
        end function
    end interface

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
        procedure(interfaceFunc1), pointer, nopass :: pp1
        procedure(interfaceFunc2), pointer, nopass :: pp2
    end type

    contains

    function func1(b)
        class(*), pointer, intent(in) :: b(:)
        type(Base(20,4)) :: func1(3,5)
        select type (b)
            type is (Base(*,4))
                func1 = reshape(b,(/3,5/),(/Base(20,4)(-1),Base(20,4)(-2)/),(/2,1/))
            class default
                error stop 1_4
        end select
    end function

    function func2(b, p)
        class(*), pointer, intent(in) :: b(:)
        procedure(interfaceFunc1), pointer, intent(in) :: p
        type(Base(20,4)) :: func2(3,5)
        func2 = p(b)
    end function
end module

program interfaceName003a
use m
    class(*), pointer :: b1(:)
    class(Child(:,4)), allocatable :: c1

    allocate(Child(20,4)::c1)
    c1%pp1 => func1
    c1%pp2 => func2

    allocate(b1(10), SOURCE=(/(Base(20,4)(i),i=1,10)/))
    print *, c1%pp2(b1, c1%pp1)
    print *, shape(c1%pp2(b1, c1%pp1))
end
