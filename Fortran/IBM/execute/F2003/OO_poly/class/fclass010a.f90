!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass010a.f
! %VERIFY: fclass010a.out:fclass010a.vf
! %STDIN:
! %STDOUT: fclass010a.out
! %EXECARGS:
! %POSTCMD:
! %END
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
    type base
        real(8) :: data = -1.d0

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        integer(8) :: id = 1

        contains

        procedure :: print => printChild
    end type

    interface operator(**)
        class (base) function power (b1, p)
        import base
            class (base), intent(in) :: b1
            integer, intent(in) :: p
            allocatable power
        end function
    end interface

    interface operator(+)
        class (base) function plus (b1, b2)
        import base
            allocatable plus
            class (base), intent(in) :: b1, b2
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        write (*, '(f10.2)') b%data
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        write (*, '(f10.2,i9)') b%data, b%id
    end subroutine
end module


class (base) function power (b1, p)
use m, only:base, child
    class (base), intent(in) :: b1
    integer, intent(in) :: p
    allocatable power

    if (p < 0) allocate (power, source=b1)

    select type (b1)
        type is (base)
            allocate (power, source=base(b1%data**p))
        type is (child)
            allocate (power, source=child(b1%data**p, b1%id**p))
        class default
            error stop 20_4
    end select
end function


class (base) function plus (b1, b2)
use m, only: base, child
    allocatable plus
    class (base), intent(in) :: b1, b2

    if (.not. same_type_as (b1, b2)) error stop 30_4

    select type (b1)
        type is (base)
            allocate (plus, source=base(b1%data + b2%data))
        type is (child)
            select type (b2)
                type is (child)
                    allocate (plus, source=child &
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
    class (base), allocatable :: b1
    class (base), pointer :: b2
    type (child) c1

    c1 = child (2.5001d0, 10)

    allocate (b1, source=child(1.5d0, 2_8))
    allocate (b2, source=child(2.1d0, 2_8))

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

