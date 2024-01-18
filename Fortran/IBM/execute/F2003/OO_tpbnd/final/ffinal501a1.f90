!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/10/2005
!*
!*  DESCRIPTION                : final sub (finalization of the unlimited poly
!                               allocatable components duringt the intrinsic
!                               assignment)
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
        integer(4) id

        contains

        final :: finalizeBase
    end type

    type, extends (base) :: child
        character(20) :: name

        contains

        final :: finalizeChild, finalizeChildArray1
    end type

    contains

    subroutine finalizeBase (b)
        type(base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildArray1 (c)
        type(child), intent(in) :: c(:)

        print *, 'finalizeChildArray1'
    end subroutine
end module

program ffinal501a1
use m
    type dataType
        class(*), allocatable :: data(:)
    end type

    type (dataType), pointer :: d1, d01, d2(:), d02(:)

    allocate (d1, d01)

    allocate (child:: d1%data(2))
    allocate (d01%data(10), source=(/(j, j=1,10)/))

    print *, 'test 1'
    d1 = d01

    allocate (d2(2), d02(2))

    allocate (base :: d2(1)%data(2))
    allocate (child :: d2(2)%data(1))

    allocate (d02(1)%data(5), source=(/(j*1.1e0_8, j=1,5)/))
    allocate (d02(2)%data (3), source= (/'abc', 'xyz', '!%$'/))

    print *, 'test 2'

    d2 = d02


    !! verify the assignment results
    if (size (d1%data) /= 10) error stop 1_4

    if ((size (d2(1)%data) /= 5) .or. (size (d2(2)%data) /= 3)) error stop 2_4

    select type (x => d1%data)
        type is (integer)
            if (any (x /= (/(j, j = 1, 10)/))) error stop 3_4
        class default
            error stop 4_4
    end select

    select type (x => d2(1)%data)
        type is (real(8))
            write (*, '(5(1x, f10.2))') x
        class default
            error stop 5_4
    end select

    select type (x => d2(2)%data)
        type is (character(*))
            print *, x
        class default
            error stop 6_4
    end select

    print *, 'end'
end
