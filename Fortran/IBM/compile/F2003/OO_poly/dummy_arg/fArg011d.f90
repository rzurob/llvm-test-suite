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
! %POSTCMD: dcomp fArg011d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (associate-name assumes
!                               the restrictions of selector)
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
        integer(4) :: id

        contains

        procedure :: assgnID => assgnBaseID
    end type

    contains

    subroutine assgnBaseID (b, id)
        class (base), intent(inout) :: b
        integer(4), intent(in) :: id

        b%id = id
    end subroutine

    subroutine test1 (b)
        class (base), intent(in) :: b

        associate (x => b)
            call x%assgnID (10)     !<-- this is illegal
        end associate
    end subroutine
end module

program fArg011d
end
