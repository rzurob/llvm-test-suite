! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : EXTENDS (renamed base type in extended type)
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
end module

module m1
use m, newBase => base
    type, extends (newBase) :: child
        character*20 :: name
    end type
end module

program fext043
use m
use m1
    type (base) :: b1 = base(10)
    type (child) :: c1 = child (11, 'c1')
    type (newBase) :: n1 = base(12)

    b1 = newBase(10)

    b1 = n1

    if (b1%id /= 12) error stop 1_4

    c1%newbase = base (12)

    if (c1%id /= 12) error stop 2_4

    c1%newbase = newBase (15)

    if (c1%id /= 15) error stop 3_4
end
