! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/21/2005
!*
!*  DESCRIPTION                : structure constructor (poly-allocatable
!                               components' allocations in structure constructor
!                               using the poly-pointer function return results
!                               as the data source)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8) id

        contains

        procedure :: makeArray2 => makeBaseArray2
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    contains

    class (base) function makeBaseArray2 (b1, shape)
        class (base), intent(in) :: b1
        integer, intent(in) :: shape(2)

        pointer makeBaseArray2 (:,:)

        allocate (makeBaseArray2(shape(1), shape(2)), source=b1)
    end function
end module


module m1
use m
    type dataType
        class (base), allocatable :: data(:,:)
    end type
end module


program fconstr035a2
use m1
    call test1 (child(1, 'test1'))

    contains

    subroutine test1 (b1)
        class (base), intent(in) :: b1

        associate (x => dataType (data = b1%makeArray2 ((/2,2/))))
            if (.not. allocated (x%data)) error stop 1_4

            if (any(shape (x%data) /= (/2,2/))) error stop 2_4

            select type (y => x%data)
                type is (child)
                    if (any (y%id /= 1)) error stop 3_4

                    if (any (y%name /= 'test1')) error stop 4_4

                class default
                    error stop 10_4
            end select
        end associate
    end subroutine
end
