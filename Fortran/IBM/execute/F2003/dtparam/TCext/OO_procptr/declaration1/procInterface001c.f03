! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/declaration1/procInterface001c.f
! opt variations: -qnol

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The procedure pointer
!                              is a dummy argument. The associated
!                              function is a module function.
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

    contains

    integer function func1(b)
        type(Base(*,4)), intent(in) :: b
        func1 = b%i * 2
    end function

    type(Base(20,4)) function func2(b, p)
        type(Base(*,4)), intent(in) :: b
        procedure(func1), pointer, intent(in) :: p
        func2 = Base(20,4)(p(b))
    end function
end module

program procInterface001c
use m
    procedure(integer), pointer :: pp1
    procedure(func2), pointer :: pp2

    pp1 => func1
    pp2 => func2

    print *, "func2", pp2(Base(20,4)(5), pp1)
end
