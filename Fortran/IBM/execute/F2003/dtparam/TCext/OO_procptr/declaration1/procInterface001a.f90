! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/declaration1/procInterface001a.f
! opt variations: -ql

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
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
    type Base(k1)    ! (4)
        integer, kind :: k1
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
            type(Base(4)) :: b
            type(Base(4)) :: func2
        end function
    end interface

    procedure(integer), pointer :: pp1
    procedure(func2), pointer :: pp2

    pp1 => func1
    pp2 => func2

    print *, "func1", pp1()
    print *, "func2", pp2(Base(4)(5))
end

integer function func1()
    func1 = 20
end function

function func2(b)
use m
    type(Base(4)) :: b
    type(Base(4)) :: func2
    func2 = Base(4)(b%i * 2)
end function
