! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (deallocate an unallocated allocatable
!                               causes an erro condition)
!
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
        class(*), pointer :: data(:)
    end type
end module

program falloc023
use m
    class (base), allocatable :: b1

    integer(4) :: error

    error = 10
    deallocate (b1, stat = error)

    if (error /= 2) error stop 1_4
end