! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/25/2005
!*
!*  DESCRIPTION                : poly-function results (result keyword use in
!                               the function definition; unlimited poly function
!                               return allocatable array)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    function makeArray (x, size) result (array)
        class(*), intent(in) :: x
        integer, intent(in) :: size

        class (*), allocatable :: array(:)

        allocate (array(size), source=x)
    end function
end module

program ffuncRet008a1
use m
    logical(4) precision_x6

    associate (x => makeArray (100_8, 10))
        if (size(x) /= 10) error stop 1_4

        select type (y => x)
            type is (integer(8))
                if (any(y /= 100)) error stop 2_4
            class default
                error stop 3_4
        end select
    end associate

    associate (x => makeArray ((1.3_8, 2.1_8), 5))
        if (size(x) /= 5) error stop 4_4

        select type (y => x)
            type is (complex(8))
                do i = 1, 5
                    if (.not. precision_x6 (y(i), (1.3_8, 2.1_8))) error stop 5_4
                end do
            class default
                error stop 6_4
        end select
    end associate
end
