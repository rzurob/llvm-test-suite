!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/31/2005
!*
!*  DESCRIPTION                : argument association (actual-arg with protected
!                               attributes to be associated with unlimited poly
!                               dummy arg; array with vector subscripts as the
!                               actual-arg)
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
        class(*), allocatable :: data(:)
    end type

    type (base), protected :: b1

    contains

    subroutine setB1 (b2)
        class (*), intent(in) :: b2(:)

        if (allocated (b1%data)) deallocate (b1%data)

        allocate (b1%data(size(b2)), source=b2)
    end subroutine

    subroutine printX (x)
        class (*), intent(in) :: x(:)

        select type (x)
            type is (integer)
                print *, x
            type is (real)
                write (*, '(10f10.2)') x
            class default
                print *, 'other type'
        end select
    end subroutine
end module

program fArg520a2
use m
    class (*), pointer :: x1(:)

    call setB1 ((/(i, i=1,5)/))

    call printX(b1%data)

    allocate (x1(3), source=(/(j*2.0, j = 1, 3)/))

    call setB1 (x1((/2,1,3/)))      !! <-- this sets b1%data to 4, 2, 6

    call printX(b1%data((/3, 1,2/)))!! <-- this prints 6, 4, 2
end
