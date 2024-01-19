! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (base component name can not be
!*                               used in the extended type, even with base type
!*                               renamed)
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
end module

module m1
use m, newBase => base
    type, extends(newBase) :: child(n)
        integer, len :: n
        character(n) :: name
    end type

    type, extends(child) :: thirdGeneration(k2)
        integer, kind :: k2
        integer(k2) :: newbase   !<-- illegal
        integer(k2) :: base      !<-- ok
    end type
end module

program fext037d2
end
