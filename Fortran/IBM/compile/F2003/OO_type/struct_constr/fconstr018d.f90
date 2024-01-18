! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (C484)
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
        integer*4 ::id
        real*4, private :: value = 1.0
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    type, extends(child) :: thirdGeneration
        logical*2 :: isSet
    end type

    type(base) :: b1_m = base (1, 10.0)
    type(child) :: c1_m = child (2, 2.0, 'c1_m')
end module

program fconstr018d
use m

    type (thirdGeneration) :: t1 = thirdGeneration (name = 't1', &
                    id = 1, base = base (2), isSet = .true.)

    type (thirdGeneration) :: t2, t3, t4

    t2 = thirdGeneration (isSet = .true., base = b1_m, name = 't2', id = 2)
    t3 = thirdGeneration (isSet = .true., child = c1_m, name = 't3')

    t4 = thirdGeneration (child = child (base = base(3), name = 't4'), &
                          isSet = .true., name = 't4')

end
