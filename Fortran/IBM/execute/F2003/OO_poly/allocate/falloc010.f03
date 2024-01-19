! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (deep copy for the allocatable
!                               components with the source-expr)
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
        integer(8), allocatable :: data(:)
    end type
end module

program falloc010
use m
    class (base), allocatable :: b1

    type (base) :: b2

    allocate (b2%data(4:6), source=(/4_8,5_8,6_8/))

    allocate (b1, source=b2)

    if ((lbound(b1%data, 1) /= 4) .or. (ubound(b1%data, 1) /= 6)) error stop 1_4

    if (any (b1%data /= (/4_8,5_8,6_8/))) error stop 2_4
end
