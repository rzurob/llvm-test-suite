! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/class/fclass010.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (user defined operator (**), and
!                               for scalars)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,8)
        integer, kind :: k1
        integer, len  :: n1
        real(k1)      :: data = -1.d0

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child    ! (20,8)
        integer(k1) :: id = 1

        contains

        procedure :: print => printChild
    end type

    interface operator(**)
        class (base(:,8)) function power (b1, p)
        import base
            class (base(*,8)), intent(in) :: b1
            integer, intent(in) :: p
            allocatable power
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base(*,8)), intent(in) :: b

        write (*, '(f10.2)') b%data
    end subroutine

    subroutine printChild (b)
        class (child(*,8)), intent(in) :: b

        write (*, '(f10.2,i9)') b%data, b%id
    end subroutine
end module


class (base(:,8)) function power (b1, p)
use m, only:base, child
    class (base(*,8)), intent(in) :: b1
    integer, intent(in) :: p
    allocatable power

    if (p < 0) allocate (power, source=b1)

    select type (b1)
        type is (base(*,8))
            allocate (power, source=base(20,8)(b1%data**p))
        type is (child(*,8))
            allocate (power, source=child(20,8)(b1%data**p, b1%id**p))
        class default
            error stop 20_4
    end select
end function


program fclass010
use m
    class (base(:,8)), allocatable :: b1
    class (base(:,8)), pointer :: b2
    type (child(20,8)) c1

    c1 = child(20,8) (2.5001d0, 10)   !<-- use 2.5001, not 2.5

    allocate (b1, source=child(20,8)(1.5d0, 2_8))

    associate (x => b1**2)
        call x%print
    end associate

    associate (x => c1**3)
        call x%print
    end associate
end

