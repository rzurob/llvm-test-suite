! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (pointers returned from function
!*                               calls are not finalized)
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
        integer*4 :: id = -1

        contains

        procedure :: replicate => replicateBase

        final :: finalizeBase
    end type

    type (base), save :: b1_m

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    function replicateBase (b)
        class (base), intent(in) :: b

        type(base), pointer :: replicateBase

        allocate (replicateBase)

        replicateBase%id = b%id
    end function

    type (base) function produceBasePtr (id)
        pointer produceBasePtr
        integer*4, intent(in) :: id

        allocate (produceBasePtr)

        produceBasePtr%id = id
    end function
end module

program ffinal514a22
use m
    class (base), pointer :: b_ptr

    print *, produceBasePtr (10)

    print *, replicateBase (produceBasePtr(2))

    b1_m = produceBasePtr (5)

    if (b1_m%id /= 5) error stop 1_4

    b_ptr => b1_m%replicate()

    if (b_ptr%id /= 5) error stop 2_4

    print *, 'end'
end
