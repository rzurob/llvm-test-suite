!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/09/2005
!*
!*  DESCRIPTION                : final sub (pointer function return containing
!                               allocatable array components)
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

        final :: finalizeBase, finalizeBaseArray1
    end type

    type, extends(base) :: child
        character(20), allocatable :: name

        contains

        final :: finalizeChild, finalizeChildArray1
    end type

    type dataType
        class (base), allocatable :: data (:)

        contains

        procedure, nopass :: genData => genDataTypePtr
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeBaseArray1'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildArray1 (c)
        type (child), intent(in) :: c(:)

        print *, 'finalizeChildArray1'
    end subroutine

    class (dataType) function genDataTypePtr (data, name, isize, csize)
        pointer genDataTypePtr(:)

        real(8), intent(in) :: data(:)
        character(*), intent(in), optional :: name
        integer, intent(in) :: isize, csize

        allocate (genDataTypePtr(isize))

        if (present(name)) then
            do i = 1, isize
                allocate (genDataTypePtr(i)%data(csize), &
                            source=child(data, name))
            end do
        else
            do i = 1, isize
                allocate (genDataTypePtr(i)%data(csize), source=base(data))
            end do
        end if
    end function
end module


program ffinal009a
use m
    class (dataType), pointer :: d1(:)

    nullify (d1)

    d1 => d1%genData ((/1.0_8, 2.0_8/), isize=3, csize=2)

    print *, 'test 1'

    deallocate (d1)

    print *, 'test 2'

    d1 => d1%genData ((/1.0_8, 2.0_8/), 'abc', 2, 3)

    deallocate (d1)

    print *, 'end'
end
