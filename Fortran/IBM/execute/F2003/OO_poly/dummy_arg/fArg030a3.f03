! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (poly allocatable
!                               components for the actual argument; test the
!                               INTENT(OUT) attribute on dummy-arg)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id = 1
    end type

    type, extends(base) :: child
        character*20 :: name = 'no-name'
    end type
end module

module m1
use m
    type container
        class (base), allocatable :: data(:)
    end type

    contains

    subroutine checkData (co)
        type (container), intent(out) :: co(5)

        do i = 1, 5
            if (allocated(co(i)%data)) then
                print *, 'data element: ', i

                error stop 1_4
            end if
        end do
    end subroutine
end module

program fArg030a3
use m1
    class(container), allocatable :: co1(:)

    allocate (co1(10))

    do i = 1, 10
        allocate (child:: co1(i)%data(i:i+1))
    end do


    call checkData (co1(2::2))

    do i = 1, 10, 2
        if (.not. allocated (co1(i)%data)) error stop 10_4

        if (allocated (co1(i+1)%data)) error stop 11_4
    end do
end