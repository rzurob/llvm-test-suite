! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/11/2005
!*
!*  DESCRIPTION                : final sub (for VALUE attribute, there is no
!                               finalizations even though an copy is used)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4, pointer :: data => null()

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
        if (associated (b%data)) deallocate (b%data)
    end subroutine
end module

program ffinal511
use m
    class (base), allocatable :: b1

    allocate (b1)

    allocate (b1%data, source=100)

    print *, 'begin'

    call test1 (b1)

    print *, 'end'

    contains

    subroutine test1 (b)
        type (base), value :: b

        print *, associated (b%data)
    end subroutine
end
