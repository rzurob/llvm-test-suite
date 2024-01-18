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
! %POSTCMD: dcomp fext037d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (base component name can not be
!*                               used in the extended type)
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
    type base(k1)
        integer, kind :: k1
    end type

    type, extends(base) :: child(n)
        integer, len :: n
        character(n) :: name
    end type

    type, extends(child) :: thirdGeneration(k2)
        integer, kind :: k2
        integer(k2) :: base       !<-- illegal
    end type

    type, extends(base) :: child2(k3)
        integer, kind :: k3
        class(*), allocatable :: base(:)    !<-- illegal
    end type
end module

program fext037d
end
