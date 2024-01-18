! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/OO_poly/point_assgn/fpAssgn031a.f
! opt variations: -ql -qreuse=none

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn031a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/21/2004
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
    type seq1(k1)    ! (4)
        integer, kind :: k1
        sequence

        integer(k1)   :: i1
        integer(k1)   :: i2
    end type
end module

program fpAssgn031a
use m
    class (*), pointer :: x

    type (seq1(4)), pointer :: s1

    allocate (x, source = seq1(4) (1, 2))

    associate (z => x)
        s1 => z

        if ((s1%i1 /= 1) .or. (s1%i2 /= 2)) error stop 1_4
    end associate

    deallocate (s1)
end
