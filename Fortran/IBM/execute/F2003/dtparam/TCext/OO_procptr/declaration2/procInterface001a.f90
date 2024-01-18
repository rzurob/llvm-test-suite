! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/OO_procptr/declaration2/procInterface001a.f
! opt variations: -qnol -qreuse=base

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The associated
!                              function is an external function.
!                              Poly and unlimited poly. Intrinsic or
!                              derived type, scalar.
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

    type, extends(Base) :: Child(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      j
    end type
end module

program procInterface001a
use m
    interface
        integer function ifunc1(b)
        use m
            class(Base(*,4)), intent(in) :: b
        end function

        real function func2(b)
        use m
            class(*), pointer :: b
        end function
    end interface

!    procedure(integer), pointer :: pp1
!    procedure(real), pointer :: pp2

    procedure(ifunc1), pointer :: pp1
    procedure(func2), pointer :: pp2

    class(*), pointer :: b1

    pp1 => ifunc1
    pp2 => func2

    print *, "ifunc1", pp1(Child(20,4,20,4)(4, 5))
    allocate(b1, SOURCE=Base(20,4)(6))
    print *, "func2", int(pp2(b1))
end

integer function ifunc1(b)
use m
    class(Base(*,4)), intent(in) :: b
    select type(b)
        type is (Base(*,4))
            ifunc1 = b%i * 2
        type is (Child(*,4,*,4))
            ifunc1 = b%i + b%j
        class default
            error stop 1_4
    end select
end function

real function func2(b)
use m
    class(*), pointer :: b
    select type(b)
        type is (Base(*,4))
            func2 = b%i / 2
        type is (Child(*,4,*,4))
            func2 = b%i - b%j
        class default
            error stop 2_4
    end select
end function
