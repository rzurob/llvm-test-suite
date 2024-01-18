!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2005
!*
!*  DESCRIPTION                : structure constructor (private parent type will
!                               result in private parent component, and is not
!                               accessible via use association)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, private :: base
        integer*4 :: id
        real*4 :: value
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    type, extends(child) :: thirdGeneration
        logical*1 :: isSet
    end type

    type (base) :: b1_m = base (1, 1.0)
    type (child) :: c1_m = child (base = base(2, 2.0), name = 'c1_m')

    type (thirdGeneration) :: t1_m = thirdGeneration ( &
            isSet = .true., child = child (base = base(3, 3.0), name = 't1_m'))
end module


program fconstr024d
use m
    type (child) :: c1, c2

    type (thirdGeneration) :: t1

    c1 = child (name = 'c1', base = base(4, 4.0))
    c2 = child (name = 'c3', base = b1_m)
    t1 = thirdGeneration (isSet = .true., child = child (base = base(5, 5.0), &
                            name = 't1'))
end
