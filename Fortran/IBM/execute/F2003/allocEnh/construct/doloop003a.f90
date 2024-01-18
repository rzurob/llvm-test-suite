!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/21/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the intrinsic assignment in a do-loop for
!                               derived type pointer that is bound-remapped by a
!                               data pointer assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
    end type

    type, extends(base) :: child
        character(:), allocatable :: name
    end type

    contains

    subroutine assgnVal (b1, intVal)
        class(base), allocatable, intent(out) :: b1
        integer, intent(in) :: intVal

        allocate(child :: b1)

        b1%id = intVal

        select type (x => b1)
            type is (base)

            type is (child)
                x%name = 'test ' // achar(65+x%id)

            class default
                stop 21
        end select
    end subroutine
end module

program doloop003a
use m

    type container
        class(base), allocatable :: data
    end type

    type(container), target :: co1(120)

    type(container), pointer :: co2(:,:)

    co2 (0:11, 0:4) => co1(::2)

    do i = 0, 11
        do j = 0, 4
            call assgnVal(co2(i,j)%data, i+j)
        end do
    end do

    !! verify result
    index1 = 1

    do j = 0, 4
        do i = 0, 11
            if (.not. allocated(co1(index1)%data)) error stop 1_4

            if (allocated(co1(index1+1)%data)) error stop 2_4

            if (co1(index1)%data%id /= i+j) error stop 3_4

            select type (x => co1(index1)%data)
                type is (child)
                    if (x%name /= 'test ' // achar(65+i+j)) error stop 4_4
                    if (x%name%len /= 6) error stop 5_4

                class default
                    error stop 6_4
            end select

            index1 = index1 + 2
        end do
    end do
end
