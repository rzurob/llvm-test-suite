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

        contains
            procedure, nopass :: value => baseValue
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    type, extends(child) :: thirdGeneration
        logical*1 :: isSet
    end type

    contains

        real*4 function baseValue ()
            baseValue = 1.0
        end function
end module


program fconstr027d
use m

    type (thirdGeneration), pointer :: t1 = thirdGeneration (1, value = 2.0, &
                name = 't1', isSet = .true.)

    t1 = thirdGeneration (isSet = .true., name='t1', id = 1, value = baseValue)
end
