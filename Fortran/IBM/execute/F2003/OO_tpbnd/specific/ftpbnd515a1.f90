! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (elemental binding call
!                               for a scalar and an array section)
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
    end type

    type, extends(base) :: child1
        character*20 :: name

        contains

        procedure, pass (c1) :: compare => compareC1C2
    end type

    type, extends(base) :: child2
        character*15 :: name

        contains

        procedure, pass (c2) :: compare => compareC1C2
    end type

    contains

    elemental logical function compareC1C2 (c1, c2)
        class (child1), intent(in) :: c1
        class (child2), intent(in) :: c2

        compareC1C2 = ((c1%name == c2%name) .and. (c1%id == c2%id))
    end function
end module

program ftpbnd515a1
use m
    type (child1) :: c1(10)
    type (child2) :: c2(2:11)

    logical verifyResult (5)

    c1 = (/(child1 (i, 'test'),i=1,10)/)

    c2 = (/(child2 (i, 'test'),i=1,10)/)

    !! check if a scalar can be used together with an array section
    verifyResult = .false.

    verifyResult (2) = .true.

    if (any(c1(2)%compare (c2(2:6)) .neqv. verifyResult)) error stop 1_4

    if (any(c2(5)%compare (c1(2::2)) .neqv. verifyResult)) error stop 2_4
end
