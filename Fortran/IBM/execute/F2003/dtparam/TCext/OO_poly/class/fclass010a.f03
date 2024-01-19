! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass010a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (test the operator precedence
!                               using ** and +)
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
    type base(k1)    ! (8)
        integer, kind :: k1
        real(k1)      :: data = -1.d0

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child    ! (8)
        integer(k1) :: id = 1

        contains

        procedure :: print => printChild
    end type

    interface operator(**)
        class (base(8)) function power (b1, p)
        import base
            class (base(8)), intent(in) :: b1
            integer, intent(in) :: p
            allocatable power
        end function
    end interface

    interface operator(+)
        class (base(8)) function plus (b1, b2)
        import base
            allocatable plus
            class (base(8)), intent(in) :: b1, b2
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base(8)), intent(in) :: b

        write (*, '(f10.2)') b%data
    end subroutine

    subroutine printChild (b)
        class (child(8)), intent(in) :: b

        write (*, '(f10.2,i9)') b%data, b%id
    end subroutine
end module


class (base(8)) function power (b1, p)
use m, only:base, child
    class (base(8)), intent(in) :: b1
    integer, intent(in) :: p
    allocatable power

    if (p < 0) allocate (power, source=b1)

    select type (b1)
        type is (base(8))
            allocate (power, source=base(8)(b1%data**p))
        type is (child(8))
            allocate (power, source=child(8)(b1%data**p, b1%id**p))
        class default
            error stop 20_4
    end select
end function


class (base(8)) function plus (b1, b2)
use m, only: base, child
    allocatable plus
    class (base(8)), intent(in) :: b1, b2

    if (.not. same_type_as (b1, b2)) error stop 30_4

    select type (b1)
        type is (base(8))
            allocate (plus, source=base(8)(b1%data + b2%data))
        type is (child(8))
            select type (b2)
                type is (child(8))
                    allocate (plus, source=child(8) &
                            (b1%data + b2%data, b1%id+b2%id))
                class default
                    error stop 32_4
            end select
        class default
            error stop 31_4
    end select
end function


program fclass010a
use m
    class (base(8)), allocatable :: b1
    class (base(8)), pointer :: b2
    type (child(8)) c1

    c1 = child(8) (2.5001d0, 10)

    allocate (b1, source=child(8)(1.5d0, 2_8))
    allocate (b2, source=child(8)(2.1d0, 2_8))

    associate (x => b2+b1**2)
        call x%print
    end associate

    associate (x => c1**3+b2)
        call x%print
    end associate

    associate (x => (b1+b2+c1)**2)
        call x%print
    end associate
end

