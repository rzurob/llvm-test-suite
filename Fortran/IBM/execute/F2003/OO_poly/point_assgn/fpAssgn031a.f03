! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (unlimited poly pointer
!                               used as selector; try sequence type pointer to
!                               point to the associate-name)
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
end module

program fpAssgn031a
use m
    class (*), pointer :: x

    type (seq1), pointer :: s1

    allocate (x, source = seq1 (1, 2))

    associate (z => x)
        s1 => z

        if ((s1%i1 /= 1) .or. (s1%i2 /= 2)) error stop 1_4
    end associate

    deallocate (s1)
end
