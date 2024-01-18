! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : EXTENDS (binding name same as the parent
!                               component name)
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
    end type

    type, extends(base) :: child
        contains

        procedure, nopass :: base => badIdea    !<-- illegal
    end type

    contains

    subroutine badIdea ()

    end subroutine
end module

program fext039d1
end
