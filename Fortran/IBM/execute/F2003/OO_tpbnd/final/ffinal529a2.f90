! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (allocated allocatable as the
!                               subobject of the actual-arg to be associated
!                               with INTENT(OUT) dummy-arg)
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

        final :: finalizeBase
    end type

    type, extends (base) :: child
        character*20 :: name

        contains

        final :: finalizeChild
    end type

    type container
        class (base), allocatable :: data
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine test1 (co)
        type (container), intent(OUT) :: co(:)
    end subroutine

    subroutine test2 (co)
        class (container), intent(OUT) :: co(:)
    end subroutine
end module

program ffinal529a2
use m
    type (container) :: co1(2)

    allocate (co1(1)%data, co1(2)%data)

    call test1 (co1)

    if ((allocated (co1(1)%data)) .or. allocated (co1(2)%data)) error stop 1_4

    print *, 'test2'

    allocate (child :: co1(1)%data, co1(2)%data)

    call test2 (co1)

    if ((allocated (co1(1)%data)) .or. allocated (co1(2)%data)) error stop 2_4

    print *, 'end'
end
