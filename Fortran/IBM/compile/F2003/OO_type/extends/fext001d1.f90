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
! %POSTCMD: dcomp fext001d1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : EXTENDS (C424)
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

program fext001d1
    type A
    end type

    type, extends(A):: DoublePrecision  !<-- this is illegal
        real(8) data
    end type

    type, extends(A) :: Real            !<-- this is illegal
    end type

    type, extends(A) :: Character       !<-- this is illegal
        character,pointer :: data
    end type

    type, extends(A) :: Integer         !<-- this is illegal
        type(integer), pointer :: data
    end type

    type, extends(A) :: Complex         !<-- this is illegal
        class(*), allocatable :: data
    end type

    type, extends(A) :: Logical         !<-- this is illegal
        type(logical), pointer :: illegal
    end type

    type, extends(A) :: byte        !<-- this is legal
        byte legal
    end type
end
