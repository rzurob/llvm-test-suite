!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/18/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the component of the associate name appears
!                               as the variable in the intrinsic assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8), allocatable :: id
    end type

    type, extends(base) :: child
        real(8), allocatable :: data(:)
    end type

    type container
        class(base), allocatable :: data
    end type

    contains

    subroutine printBase (b)
        class(base), intent(in), allocatable :: b

        if (allocated(b)) then
            select type (b)
                type is (base)
                    if (allocated(b%id)) print *, b%id

                class is (child)
                    if (allocated(b%id)) write (*, '(i10,1x)', advance='no') b%id

                    if (allocated(b%data)) then
                        write (*, '(a, i10, 1x, i10, 1x)', advance='no') &
                            'bounds: ', lbound(b%data), ubound(b%data)

                        write (*, '(10(g15.8, 1x))') b%data

                    else
                        print *, 'data not allocated'
                    end if

                class default
                    error stop 10_4
            end select
        end if
    end subroutine

end module

program selectType002
use m
    class (container), pointer :: co1(:)
    double precision d1(0:99)

    allocate (co1(10))

    d1 = log((/(i*1.0d2, i=1,100)/))

    !! first block is to allocate components
    select type (co1)
        type is (container)
            do i = 1, 10, 2
                allocate (co1(i)%data)
                allocate (child :: co1(i+1)%data)
            end do

        class default
            error stop 1_4

    end select

    !! select block is to set the components' values
    do i = 1, 10
        select type (x => co1(i)%data)
            class is (base)
                x%id = 2**i

            type is (child)
                x%id = i
                x%data = d1(0:i**2-1:i)

            class default
                error stop 2_4
        end select
    end do

    do i = 1, 10
        call printBase (co1(i)%data)
    end do
end
