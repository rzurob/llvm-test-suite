! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (derived type with
!*                               private component)
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
        real*4, private :: value = 0.0
    end type

    type (base), save :: b1_m = base (1, 10.0)

    contains

    logical function isBaseCorrect (b, intVal, realVal)
        type (base), intent(in) :: b
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal

        isBaseCorrect = ( (b%id == intVal) .and. (b%value == realVal) )
    end function
end module

program fconstr013
use m

    type (base) :: b1 = base (1)
    type (base) :: b2 = base (id = 2)

    if (.not. isBaseCorrect (b1, 1, 0.0)) error stop 1_4
    if (.not. isBaseCorrect (b2, 2, 0.0)) error stop 2_4

    if (.not. isBaseCorrect (b1_m, 1, 10.0)) error stop 3_4
end