! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_procptr/declaration2/procInterface002e.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Do not specify proc-interface. The
!                              associated function is an internal
!                              function. Poly and unlimited poly.
!                              Intrinsic or derived type, scalar.
!
!                              This is a diagnostic test case.
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

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type
end module

program procInterface002e
use m
    implicit type(Base(20,4)) (p)

    procedure(integer), pointer :: ipp1
    procedure(func2), pointer :: pp2
    class(Base(:,4)), pointer :: b1
    class(*), allocatable :: b2

    ipp1 => func1
    pp2 => func2

    allocate(b1, SOURCE=Child(20,4)(3,4))
    print *, "func1", ipp1(b1)

    allocate(b2, SOURCE=Base(20,4)(5))
    print *, "func2", pp2(b2)

    contains

    integer function func1(b)
        class(Base(*,4)), intent(in) :: b
        select type (b)
            type is (Base(*,4))
                func1 = b%i
            type is (Child(*,4))
                func1 = b%i+b%j
            class default
                error stop 1_4
        end select
    end function

    type(Base(20,4)) function func2(b)
        class(*), allocatable, intent(in) :: b
        select type (b)
            type is (Base(*,4))
                func2 = Base(20,4)(-b%i)
            type is (Child(*,4))
                func2 = Base(20,4)(b%i-b%j)
            class default
                error stop 2_4
        end select
    end function
end
