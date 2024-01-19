! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/15/2005
!*
!*  DESCRIPTION                : final sub (deallocation of allocated
!                               allocatable subobject during deallocate)
!*
!*  KEYWORD(S)                 :
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

        contains

        final :: finalizeBase
    end type

    type container
        class (base), allocatable :: data
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal533
use m
    type (container), pointer :: co1(:)

    allocate (co1(2))
    allocate (co1(1)%data, co1(2)%data)

    deallocate (co1)

    print *, 'end'
end
