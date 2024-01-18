! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/11/2005
!*
!*  DESCRIPTION                : final sub (step3 during the finalization
!                               process kick off the finalizations again for the
!                               parent components)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        real(8), pointer :: data(:) => null()

        contains

        final :: finalizeAarray1, finalizeA
    end type

    type base
        type (A) a1(2)
    end type

    type, extends(base) :: child
        character(20), pointer :: name(:) => null()

        contains

        final :: finalizeChild, finalizeChildArray1
    end type

    contains

    subroutine finalizeA (a1)
        type (A), intent(inout) :: a1

        if (associated (a1%data)) then
            print *, 'deallocating data'

            deallocate (a1%data)
        end if
    end subroutine

    subroutine finalizeAarray1 (a1)
        type (A), intent(inout) :: a1(:)

        print *, 'finalizeAarray1'

        do i = 1, size(a1)
            call finalizeA (a1(i))
        end do
    end subroutine

    subroutine finalizeChild(c)
        type (child), intent(inout) :: c

        if (associated (c%name)) then
            print *, 'deallocating name'

            deallocate (c%name)
        end if
    end subroutine

    subroutine finalizeChildArray1 (c)
        type (child), intent(inout) :: c(:)

        print *, 'finalizeChildArray1'

        do i = 1, size(c)
            call finalizeChild (c(i))
        end do
    end subroutine
end module

program ffinal008a
use m
    class (base), pointer :: b1, b2(:)

    allocate (child :: b1, b2(2))

    !! allocate the data components
    allocate (b1%a1(1)%data(2), b1%a1(2)%data(2))

    allocate (b2(1)%a1(1)%data(1:0), b2(2)%a1(1)%data(2), b2(2)%a1(2)%data(1))

    deallocate (b1)
    deallocate (b2)

    print *, 'end'
end
