! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (unlimited-poly data
!                               used as SOURCE in transfer())
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
        integer(4) :: id
    end type

    type, extends(base) :: child
        character(15) :: name
    end type
end module

program fpAssgn501
use m
    class (*), pointer :: x (:)

    type (child), target :: c1(2:3), c2(2)

    c1%id = (/1,2/)
    c1%name = (/'c1_1', 'c1_2'/)

    x => c1

    c2 = transfer (x, c1,2)

    if (any (c2%id /= c1%id)) error stop 1_4

    if (any (c2%name /= c1%name)) error stop 2_4
end