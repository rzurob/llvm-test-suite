! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (binding name is same as
!*                               the type name, can't be extended)
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

        contains

        procedure :: child => badIdea
    end type

    contains

    subroutine badIdea (c)
        class (child(4,*)) :: c
    end subroutine

end module

program fext038d3
use m

    type, extends(child) :: t(k2)   !<-- illegal
        integer, kind :: k2
        logical(k2) :: isSet
    end type

end
