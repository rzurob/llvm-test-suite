! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/declaration1/procInterface002a.f
! opt variations: -ql

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Do not specify proc-interface. The
!                              associated procedure is either an
!                              external function or an external
!                              subroutine. Non-poly. Intrinsic or
!                              derived type, scalar.
!
!                              Use default implicit typing to match
!                              the procedure pointer with the target.
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

program procInterface002a
use m
    procedure(), pointer :: pp1
    procedure(func1), pointer :: pp2

    interface
        subroutine sub1(i)
            integer, intent(in) :: i(2,3)
        end subroutine

        function func1(b)
        use m
            type(Base(4)), intent(in) :: b
            real :: func1
        end function
    end interface

    pp1 => sub1
    pp2 => func1

    call pp1((/(i,i=1,6)/))
    print *, "func1", int(pp2(Base(4)(5)))
end

subroutine sub1(i)
    integer, intent(in) :: i(2,3)
    print *, "sub1", i
end subroutine

function func1(b)
use m
    type(Base(4)), intent(in) :: b
    real :: func1
    func1 = b%i * 2
end function