! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-01-11
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : defect 380877: coarray section actual
!                               can NOT be associated with an explicit-shape
!                               coarray dummy, or assumed-size coarray.
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

program d380877
    real, save :: arr(20)[*]

    call foo (arr(::2))  !<-- this couldn't happen
    i =  bar (arr(::3))  !<-- this couldn't happen

    contains

    subroutine foo (x)
        real, intent(inout) :: x(10)[*]
    end subroutine

    integer function bar (x)
        real, intent(in) :: x(0:*)[0:*]

        bar = x(0)
    end function
    end
