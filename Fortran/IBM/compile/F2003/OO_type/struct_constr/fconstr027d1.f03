! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (C487)
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
        integer*4 :: id
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    type, extends(child) :: thirdGeneration
        logical*1 :: isSet
    end type
end module


program fconstr027d1
use m

    type (thirdGeneration) :: t1

    t1 = thirdGeneration (isSet = .true., name='t1', t1%id = 1)   !<-- illegal
end
