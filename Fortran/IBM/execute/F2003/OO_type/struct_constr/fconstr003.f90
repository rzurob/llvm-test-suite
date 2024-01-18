! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (default construction)
!*                               no initialization for any component
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
        real*4, pointer :: value
    end type

    type, extends(base) :: child
        character(20) :: name
        real*8, allocatable :: extendedValue (:)
    end type

    type (base), save :: b1_m
    type (child), save :: c1_m

end module

program fconstr003
use m

    type, extends (base) :: secondChild
        real*4, pointer :: extendedValue (:)
    end type

    type (base) :: b1
    type (child) :: c1
    type (secondChild) :: s1

end
