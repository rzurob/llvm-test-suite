! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/25/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (selector's restrictions will be
!                               applied to associate-name: IO and intrinsic
!                               assignment on poly-entity is not allowed)
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
        integer*4 :: id = 1
    end type
end module

program fclass008d1
use m
    class (base), allocatable :: b1

    allocate (b1)

    associate (x => b1)
        print *, x%id, x    !<-- illegal for IO

        x = base (10)       !<-- illegal for intrinsic assignment
    end associate
end
