! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/21/2005
!*
!*  DESCRIPTION                : structure constructor (the poly allocatable
!                               components' allocations in structure
!                               constructor for function result as the data
!                               source)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8) id
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    contains

    type (base) function makeBaseArray2 (b1, shape)
        class (base), intent(in) :: b1
        integer, intent(in) :: shape(2)

        dimension makeBaseArray2 (2:shape(1)+1, 2:shape(2)+1)

        makeBaseArray2%id = b1%id
    end function
end module


module m1
use m
    type dataType
        class (base), allocatable :: data(:,:)
    end type
end module


program fconstr035a3
use m1
    call test1 (child(1, 'test1'))

    contains

    subroutine test1 (b1)
        class (base), intent(in) :: b1

        associate (x => dataType (data = makeBaseArray2 (b1, (/3,3/))))
            if (.not. allocated (x%data)) error stop 1_4

            if (any(shape (x%data) /= (/3,3/))) error stop 2_4

            if (any (lbound(x%data) /= 1)) error stop 3_4
            if (any (ubound(x%data) /= 3)) error stop 4_4

            select type (y => x%data)
                type is (base)
                    if (any (y%id /= 1)) error stop 5_4

                class default
                    error stop 10_4
            end select
        end associate
    end subroutine
end
