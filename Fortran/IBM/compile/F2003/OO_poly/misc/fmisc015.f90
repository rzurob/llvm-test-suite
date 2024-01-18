!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fmisc015.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 293022)
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

program fmisc015
    integer, pointer :: j

    associate (x => 10)
        allocate (j, stat = x)      !<-- illegal

        print *, x
        print *, (/(x, x = 1, 3)/)  !<-- illegal
        read *, x                   !<-- illegal
        print *, x

        call test1 (x)              !<-- illegal

        print *, x
    end associate

    contains

    subroutine test1 (ii)
        integer, intent(inout) :: ii

        ii = ii * 2
    end subroutine

end

