!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2005
!*
!*  DESCRIPTION                : structure constructor (the use of structure
!                               constructor is illegal for a type made
!                               accessible via use association and contains
!                               private component that is not default
!                               initialized)
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
        integer*4 ::id
        real*4, private :: value
    end type

    type, extends (base) :: child
        character(20) :: name
    end type

    type (child) :: c1_m = child (1, 1.0, 'c1_m') ! this is OK

end module

program fconstr014d2
use m
    type (child) :: c2 = child (id = 3, name = 'c2')  ! this is illegal
end
