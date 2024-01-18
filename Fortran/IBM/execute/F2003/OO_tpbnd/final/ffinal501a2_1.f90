! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/10/2005
!*
!*  DESCRIPTION                : final sub (finalization of allcoatable
!                               components during intrinsic assignment)
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
        real(8), allocatable :: data(:)

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child
        character(3), allocatable :: name(:)

        contains

        final :: finalizeChild, finalizeChildArray1
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        if (allocated (b%data)) deallocate (b%data)
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c

        print *, 'finalizeChild'

        if (allocated (c%name)) deallocate (c%name)

        call finalizeBase (c%base)
    end subroutine

    subroutine finalizeChildArray1 (c)
        type (child), intent(inout) :: c(:)

        print *, 'finalizeChildArray1'

        do i = 1, size (c)
            call finalizeChild (c(i))
        end do
    end subroutine
end module

program ffinal501a2_1
use m
    type dataType
        class (base), allocatable :: data(:)
    end type

    type (dataType) d1, d01


    allocate (d01%data(2), source=(/child((/1.1_8, 2.1_8/), (/'abc', 'xyz'/)), &
            child ((/3.1_8, 4.0_8/), (/'ABC', 'XYZ'/))/))

    d1 = d01

    print *, 'test 1'

    d1 = d01

    select type (x => d1%data)
        type is (child)
            if (size(x) /= 2) error stop 2_4

            write (*, '(4(1x, f10.2))') x(1)%data, x(2)%data

            print *, x(1)%name, x(2)%name
        class default
            error stop 1_4
    end select
end
