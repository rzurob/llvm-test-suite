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
    type base
    end type
end module

module m1
use m, newBase => base
    type, extends(newBase) :: child
        character(20) :: name
    end type

    type, extends(child) :: thirdGeneration
        integer*4 :: newbase   !<-- illegal
        integer*4 :: base      !<-- OK
    end type
end module

program fext037d2
end