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
! %POSTCMD: dcomp fext038d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (base type name is the same as
!*                               that of a component, can't be extended)
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

module m1
    type base
        integer*4 :: base
    end type
end module

module m
    type, private :: base
        integer*4 :: base = 1
    end type

    type, extends(base) :: child    !<-- illegal to extend base
        character(20) :: name
    end type

end module

program fext038d
use m1

    type, extends(base) :: child    !<-- illegal to extend base
    end type
end
