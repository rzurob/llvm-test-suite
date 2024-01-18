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
! %POSTCMD: dcomp fconstr012d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (C484)
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
        integer*4 ::id = 1
        real*4 :: value = 0.0
    end type

    type, extends(base) :: child
        character*20 :: name = 'test'
    end type

    type, extends(child) :: thirdGeneration
        logical*1 :: isSet = .false.
    end type
end module

program fconstr012d
use m

    type (base) :: b1
    type (child) :: c1, c2
    type (thirdGeneration) :: t1

    t1 = thirdGeneration (child = c1, name = 't1')
end
