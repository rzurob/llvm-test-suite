! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (allocatable function result
!*                               deallocated after its use)
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

        contains

        procedure :: replicate => replicateBase
        final :: finalizeBase
    end type

    contains

    function replicateBase (b)
        class (base), intent(in) :: b
        type (base), allocatable :: replicateBase

        allocate (replicateBase)

        replicateBase%id = b%id
    end function

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine
end module

use m
    type (base), save :: b1

    b1%id = 10

    if (.not. (allocated (b1%replicate()))) error stop 1_4

    print *, allocated (b1%replicate())
    print *, 'end'
end
