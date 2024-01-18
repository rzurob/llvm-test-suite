!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2005
!*
!*  DESCRIPTION                : structure constructor (diagnostic test case
!                               for re-using component that is inherited)
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
    type base
    end type

    type, extends (base) :: child
    end type

    type, extends (child) :: thirdGeneration
        integer*4 :: id
    end type
end module

program fconstr008d
use m

    type (thirdGeneration) :: t1 = thirdGeneration (base = base(), &
                child = child (), id = 2)

    !! strange as it appears, the next structure constructor is legal
    type (thirdGeneration) :: t2 = thirdGeneration (2, base = base())

    type (thirdGeneration) :: t3 = thirdGeneration (child = child(), 3)
end
