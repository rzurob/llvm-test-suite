! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/07/2005
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) for pointer
!                               dummy-arg just makes the actual-arg undefined;
!                               no finalization occurrs)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class(*), allocatable :: x

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        if (allocated (b%x)) deallocate (b%x)
    end subroutine

    subroutine test1 (b1)
        class (base), pointer, intent(out) :: b1

        allocate (b1)
    end subroutine

    subroutine test2 (b1)
        type (base), pointer, intent(out) :: b1

        allocate (b1)
    end subroutine
end module

program fArg512
use m
    class (base), pointer :: b11
    type (base), pointer :: b22

    allocate (b11, b22)

    allocate (b11%x, source= 100)
    allocate (base::b22%x)

    print *, 'begin'

    call test1(b11)
    call test2 (b22)

    if ((.not. associated (b11)) .or. (.not. associated (b22))) error stop 1_4

    if (allocated (b11%x) .or. allocated(b22%x)) error stop 2_4

    print *, 'end'
end