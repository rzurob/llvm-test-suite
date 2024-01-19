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
    type base(k1,n)
        integer, kind :: k1
        integer, len :: n
    end type

    type, extends(base) :: child(k2)
        integer, kind :: k2
        contains

        procedure, nopass :: base => badIdea    !<-- illegal
    end type

    contains

    subroutine badIdea ()

    end subroutine
end module

program fext039d1
end
