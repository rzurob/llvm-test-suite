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

program ffuncRet008a2
use m
    type base
        character(20), allocatable :: name(:)
    end type

    associate (x => makeArray (base((/'abc', 'xyz', '123'/)), 10))
        if (size(x) /= 10) error stop 1_4

        select type (y => x)
            type is (base)
                do i = 1, 10
                    if (.not. allocated(y(i)%name)) error stop 2_4

                    if (any (y(i)%name /= (/'abc', 'xyz', '123'/))) error stop 3_4
                end do
            class default
                error stop 5_4
        end select
    end associate
end