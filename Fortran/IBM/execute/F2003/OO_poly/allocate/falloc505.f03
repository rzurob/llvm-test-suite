! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE/DEALLOCATE (pointer associated with an
!                               allocatable target shall not be deallocated)
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
        integer(4) id
    end type
end module

program falloc505
use m
    class (*), pointer :: x, x1(:)

    integer, target, allocatable :: i1
    class (base), target, allocatable :: b1(:)
    complex(4), target, allocatable :: c1(:)
    integer error

    allocate (i1, b1(2), c1(3))

    x => i1

    x1 => b1

    error = 0
    deallocate (x, stat=error)

    if (error /= 2) error stop 1_4

    error = 0
    deallocate (x1, stat=error)

    if (error /= 2) error stop 2_4

    x1 => c1

    error = 0
    deallocate (x1, stat=error)

    if (error /= 2) error stop 3_4

    deallocate (i1, c1, b1, stat=error)

    if (error /= 0) error stop 4_4
end