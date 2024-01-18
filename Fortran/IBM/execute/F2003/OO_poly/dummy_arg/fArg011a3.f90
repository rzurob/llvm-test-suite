! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (tests a rank-2 array as
!                               selector in ASSOCIATE construct)
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

    type, extends (base) :: child
        character(20) :: name
    end type
end module

module m1
use m

    contains

    integer(4) function IDSum (b)
        type (base), intent(in) :: b (2,2)

        associate (x => b)
            IDSum = sum (x%id)
        end associate
    end function

    integer(4) function SumID (b)
        class (base), intent(in) :: b(2,3)

        associate (x => b)
            SumID = sum (x%id)
        end associate
    end function
end module

program fArg011a3
use m1
    class (base), allocatable :: b1 (:)

    allocate (b1 (10), source=child (1, 'b1_array'))

    b1%id = (/(i, i=1,10)/)

    if (IDSum (b1(2:5)) /= 14) error stop 1_4

    if (SumID (b1(2:)) /= 27) error stop 2_4

    deallocate (b1)
end
