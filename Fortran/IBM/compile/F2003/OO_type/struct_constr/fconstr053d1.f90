! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (C488)
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
        integer*4, private :: id = 1
        character*20 :: name
    end type
end module

program fconstr053d1
use m
    type (base) :: b1 = base (name = "b1")

    b1 = base (id = 10, name = 'b1')   !<-- id is inaccessible

    b1 = base (10, name = 'b1')        !<-- a different error message
end
