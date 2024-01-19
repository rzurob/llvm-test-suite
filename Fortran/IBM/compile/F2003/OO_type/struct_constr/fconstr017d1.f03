! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (C483, C485, C487)
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

end module

program fconstr017d1
use m

    type (child) :: c1 = child (name = 'c1', value = 1.0, id = 10)
    type (child) :: c2 = child (name = 'c1', id = 10, id = 20)

    type (thirdGeneration) :: t1 = thirdGeneration ( &
                    value = 0.0, isSet=.true., name = 't1', id = 1)

    type (thirdGeneration) :: t2, t3

    t2 = thirdGeneration (name = 't2', id = 2, isSet=.true., name = 't2')
    t3 = thirdGeneration (name = 't3', id =3)

end
