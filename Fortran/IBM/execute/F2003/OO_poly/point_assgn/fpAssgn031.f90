!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn031.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (unlimited poly pointer
!                               used as selector; try sequence type pointer to
!                               point to the associate-name)
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
    type seq1
        sequence

        integer*4 :: i1
        integer*4 :: i2
    end type
end module

program fpAssgn031
use m
    class (*), pointer :: x

    type (seq1), pointer :: s1

    type (seq1), target :: s2

    s2 = seq1 (1, 2)

    x => s2

    associate (z => x)
        s1 => z

        if ((s1%i1 /= 1) .or. (s1%i2 /= 2)) error stop 1_4
    end associate

end
