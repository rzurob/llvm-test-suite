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
    type base(k1)
        integer, kind :: k1
        integer(k1) :: base
    end type
end module

module m
    type, private :: base(k2)
        integer, kind :: k2
        integer(k2) :: base = 1
    end type

    type, extends(base) :: child(n)    !<-- illegal to extend base
        integer, len :: n
        character(n) :: name
    end type

end module

program fext038d
use m1

    type, extends(base) :: child(n)    !<-- illegal to extend base
        integer, len :: n
    end type
end
