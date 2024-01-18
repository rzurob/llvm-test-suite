!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2005
!*
!*  DESCRIPTION                : structure constructor (diagnostic test case:
!                               NOTE the 1st error message in the vf file may
!                               get changed later)
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
        real*4, private :: value = 0.0
    end type

    type, extends (base) :: child
        character(20) :: name
    end type

    type (child) :: c1_m = child (1, 1.0, 'c1_m') ! this is OK

end module

program fconstr014d
use m

    !! the following two declarations are illegal
    type (child) :: c1 = child (2, 1.0, 'c1')
    type (child) :: c2 = child (id = 3, value = 1.0, name = 'c2')

end
