! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/22/2005
!*
!*  DESCRIPTION                : structure constructor (rank two allocatable
!                               array component in structure constructor)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType
        class (*), allocatable :: data(:)
    end type

    type container
        type (dataType), allocatable :: value(:,:)
    end type
end module

program fconstr050a4
use m
    type, extends(dataType) :: mainData
        integer(4) id
    end type

    class (*), allocatable :: x1(:), x2(:)

    class (dataType), allocatable :: d1(:,:)

    allocate (x1(2), source=(/10, 20/))
    allocate (x2(0:2), source=(/1.5, 3.2, 2.4/))

    allocate (d1(-1:0,0:1), source=reshape((/mainData(x1, 1), mainData(x1, 2), &
                   mainData(x2,3), mainData(x2, 4)/), (/2,2/)))

    associate (x => container (d1))
        if (.not. allocated (x%value)) error stop 1_4

        if (any(lbound(x%value) /= (/-1,0/))) error stop 2_4
        if (any(ubound(x%value) /= (/0, 1/))) error stop 3_4

        do i = -1, 0
            do j = 0, 1
                print *, '(', i,j, ')', 'bounds:', lbound(x%value(i,j)%data), &
                                        ubound(x%value(i,j)%data)

                select type (y => x%value(i,j)%data)
                    type is (integer)
                        print *, y
                    type is (real)
                        write (*, '(3(f10.2))') y
                    class default
                        error stop 10_4
                end select
            end do
        end do
    end associate
end
