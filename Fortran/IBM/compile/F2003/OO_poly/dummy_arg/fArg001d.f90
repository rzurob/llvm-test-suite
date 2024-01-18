! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (C401 or C482: abstract
!                               nonpoly data can not be declared to be with type
!                               abstract)
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
    type, abstract :: base
        integer*4 :: i
    end type

    type, extends (base) :: child
        character*20 :: name
    end type

    contains

    subroutine abc (b)
        type (base) :: b  !<-- this is illegal
    end subroutine
end module

program fArg001d
end
