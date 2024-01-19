! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/21/2005
!*
!*  DESCRIPTION                : structure constructor (poly-allocatable
!                               components' allocations in structure constructor
!                               using the function results as the data source)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8) :: id
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    contains

    class (base) function makeBaseAlloc (id, begin, isize, name)
        integer(8), intent(in) :: id
        integer(4), intent(in) :: begin, isize
        character(*), optional, intent(in) :: name(isize)

        allocatable makeBaseAlloc (:)

        if (present (name)) then
            allocate (makeBaseAlloc (begin:begin+isize-1), &
                      source=(/(child(id+j, name(j)), j=1, isize)/))
        else
            allocate (makeBaseAlloc (begin:begin+isize-1), &
                      source=(/(base (id+j), j = 1, isize)/))
        end if
    end function
end module

program fconstr035a
use m
    type dataType
        class (base), allocatable :: data1(:)
        class (base), allocatable :: data2(:,:)
    end type

    associate (x => dataType (data1 = makeBaseAlloc(1_8, 10_4, 2_4), &
                    data2 = reshape (makeBaseAlloc (100_8, 1_4, 4_4, &
                            (/'d1', 'd2', 'd3', 'd4'/)), (/2,2/))))

        if ((.not. allocated (x%data1)) .or. (.not. allocated (x%data2))) &
                error stop 1_4

        if ((lbound (x%data1, 1) /= 1) .or. (ubound(x%data1, 1) /= 2)) &
                error stop 2_4

        if ((any(lbound (x%data2) /= 1)) .or. (any (ubound(x%data2) /= 2))) &
                error stop 3_4


        !! verify the results
        select type (y => x%data1)
            type is (base)
                if (any(y%id /= (/2, 3/))) error stop 4_4
            class default
                error stop 5_4
        end select

        select type (y => x%data2)
            type is (child)
                if (any (y%id /= reshape ((/101, 102, 103, 104/), (/2,2/)))) &
                        error stop 6_4

                if (any (y%name /= reshape ((/'d1', 'd2', 'd3', 'd4'/), &
                            (/2,2/))))  error stop 7_4
            class default
                error stop 8_4
        end select
    end associate
end
