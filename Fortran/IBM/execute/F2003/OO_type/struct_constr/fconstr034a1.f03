! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (test the unlimited poly
!                               allocatable array component in structure
!                               constructor)
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
        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        real(8) r1
        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, 'base type is empty'
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        write (*, '(d10.3)') b%r1
    end subroutine
end module


program fconstr034a1
use m
    type container
        class(*), allocatable :: data(:)
    end type

    class (base), pointer :: b1(:)
    class (base), allocatable :: b2(:)

    type (child), target :: c1(2:10)

    c1%r1 = (/(j*1.1d0, j=2, 10)/)

    b1 => c1

    allocate (b2(-1:0), source=(/child(-1), child(0)/))


    !! test the array
    associate (x1 => container (b1), x2 => container(b2))
        if ((.not. allocated (x1%data)) .or. (.not. allocated(x2%data))) &
                    error stop 1_4

        !! test the bounds
        if ((lbound(x1%data,1) /= 2) .or. (ubound(x1%data,1) /= 10)) error stop 2_4
        if ((lbound(x2%data,1) /= -1) .or. (ubound(x2%data,1) /= 0)) error stop 3_4

        print *, 'x1'
        select type (y1 => x1%data)
            class is (base)
                do i = lbound(y1,1), ubound(y1,1)
                    call y1(i)%print
                end do
            class default
                error stop 4_4
        end select

        print *, 'x2'
        select type (y2 => x2%data)
            type is (child)
                do i = lbound(y2,1), ubound(y2,1)
                    call y2(i)%print
                end do
            class default
                error stop 5_4
        end select
    end associate


    !! test the array sections
    b1 => c1(::3)           !<-- c1(2), (5), (8)

    associate (x3 => container (b1), x4 => container (b2(0:)))
        if ((.not. allocated (x3%data)) .or. (.not. allocated(x4%data))) &
                    error stop 10_4

        if ((lbound(x3%data, 1) /= 1) .or. (ubound(x3%data, 1) /= 3)) &
                    error stop 11_4

        if ((lbound(x4%data, 1) /= 1) .or. (ubound(x4%data, 1) /= 1)) &
                    error stop 12_4


        print *, 'x3'

        select type (y1 => x3%data)
            type is (child)
                do i = 1, 3
                    call y1(i)%print
                end do
            class default
                error stop 13_4
        end select

        print *, 'x4'

        select type (y2 => x4%data)
            class is (base)
                call y2(1)%print
            class default
                error stop 14_4
        end select
    end associate
end