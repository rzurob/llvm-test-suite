!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftpbnd508a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (inherited binding and
!*                               overridden binding for two extended types)
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
        integer*4 :: value

        contains

        procedure :: compare => aGtB
    end type

    contains

    !! for the base type, returns true only if the caller has a larger value
    logical function aGtB (a, b)
        class (base), intent(in) :: a, b

        aGtB = (a%value > b%value)
    end function

end module

module m1
use m
    type, extends (base) :: child1
        character(20) :: name
    end type

    type, extends (base) :: child2
        logical :: flag

        contains

        procedure :: compare => aLeb
    end type

    contains

    !! for child2 type, returns true only the caller has a smaller/equal value
    logical function aLeb (a, b)
        class (child2), intent(in) :: a
        class (base), intent(in) :: b

        aLeb = (a%value <= b%value)
    end function
end module

program ftpbnd508a
use m1
    type (child1) :: c1

    type (child2) :: c2

    c1 = child1 (10, 'c1')

    c2 = child2 (20, (1==1))

    if (c1%compare(c2)) error stop 1_4

    if (c2%compare(c1)) error stop 2_4
end
