! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : struture constructor (data-target shall be
!*                               allowable for pointer component as if in a data
!*                               pointer assignment)
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
    type seq1
        sequence
        integer*4 :: i1
        integer*4 :: i2
    end type

    type base
        type (seq1), pointer :: s1
    end type

    class (*), pointer :: x
end module

program fconstr500
use m
    type (seq1), target :: s1
    type (base) :: b1

    s1 = seq1 (1, 2)

    x => s1

    b1 = base (s1 = x)

    if (.not. associated (b1%s1, s1)) error stop 1_4

    if ((b1%s1%i1 /=1) .or. (b1%s1%i2 /= 2)) error stop 2_4
end
