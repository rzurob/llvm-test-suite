! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited
!*                               poly-allocatable actual-arg associated with
!*                               dummy-arg; unallocated actual-arg allowed)
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
    contains

    subroutine deallocateX (x)
        class (*), allocatable, intent(inout) :: x

        if (allocated (x)) then
            deallocate (x)
        else
            print *, 'not allocated'
        end if
    end subroutine
end module

program fArg006a1
use m
    class (*), allocatable :: x1

    print *, 'first call'

    call deallocateX (x1)

    allocate (integer*4 :: x1)

    print *, 'second call'

    call deallocateX (x1)

    print *, 'last call'

    call deallocateX (x1)
end
