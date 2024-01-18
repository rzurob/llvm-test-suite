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
! %POSTCMD: dcomp fconstr017d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (C483)
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
        integer*4 ::id
        real*4, private :: value = 1.0
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    type, extends(child) :: thirdGeneration
        logical*2 :: isSet
    end type

    type (thirdGeneration) :: t1_m = thirdGeneration (id = 10, name = 't1_m', &
            isSet=.true., value = 1.0, name = 't1_m')

end module

program fconstr017d

end
