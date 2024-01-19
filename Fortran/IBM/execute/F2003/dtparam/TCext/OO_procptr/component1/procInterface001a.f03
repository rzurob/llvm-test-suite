! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/component1/procInterface001a.f
! opt variations: -qnok -qnol
! with manual adjustment (explicit interface in line 52)

!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The associated
!                              function is an external function.
!                              Non-poly. Intrinsic or derived type,
!                              scalar.
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
end module

program procInterface001a
use m
    interface
        integer function func1()
        end function

        function func2(b)
        use m
            type(Base(*,4)), intent(in) :: b
            type(Base(20,4)) :: func2
        end function
    end interface

    type Container(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        procedure(integer), nopass, pointer :: pp1
        procedure(func2), nopass, pointer :: pp2
    end type

    type(Container(4,20)) :: c1
    c1%pp1 => func1
    c1%pp2 => func2

    print *, "func1", c1%pp1()
    print *, "func2", c1%pp2(Base(20,4)(5))
end

integer function func1()
    func1 = 20
end function

function func2(b)
use m
    type(Base(*,4)), intent(in) :: b
    type(Base(20,4)) :: func2
    func2 = Base(20,4)(b%i * 2)
end function
