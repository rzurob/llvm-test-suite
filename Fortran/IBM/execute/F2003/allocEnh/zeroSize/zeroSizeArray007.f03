! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/29/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Use the finalizer for the derived type for the
!                               LHS allocatable variables.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), pointer :: data => null()

        contains

        final :: finalizeBase, finalizeBaseRank1, finalizeBaseRank2
    end type

    contains

    subroutine finalizeBase (b)
        type(base), intent(inout) :: b

        print *, 'finalizeBase'

        if (associated(b%data)) deallocate (b%data)
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type(base), intent(inout) :: b(:)

        print *, 'finalizeBaseRank1; size =', size(b)

        do i = 1, size(b)
            if (associated(b(i)%data)) deallocate(b(i)%data)
        end do
    end subroutine

    subroutine finalizeBaseRank2 (b)
        type(base), intent(inout) :: b(:,:)

        print *, 'finalizeBaseRank2; shape =', shape(b)

        do i = 1, size(b,1)
            do j = 1, size(b,2)
                if (associated(b(i,j)%data)) deallocate(b(i,j)%data)
            end do
        end do
    end subroutine
end module

program zeroSizeArray007
use m
    type (base), allocatable :: b1, b2(:), b3(:,:)
    type (base) :: b4, b5(10), b6(3,2)

    print *, 'test 1'

    b1 = base()

    print *, 'test 2'

    b2 = b5

    print *, 'test 3'

    b3 = b6

    print *, 'test 4'

    b3 = b6(:, 2:1)

    print *, 'test 5'

    b2 = b5(10:1)

    print *, 'deallocate b2, b3'
    deallocate (b2)
    deallocate (b3)

    print *, 'last test'

    b1 = b4

    print *, 'end'
end