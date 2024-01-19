! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (allocation of unlimited poly
!                               allocatable array using derived type as the
!                               source-expr; use select type to verify)
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
    type, abstract :: base
    end type

    type, extends(base) :: child
        integer(4) id
        character(21) :: name
    end type

    contains

    subroutine printX (x)
        class (*), allocatable, intent(in) :: x(:)

        if (.not. allocated(x)) error stop 1_4

        print *, 'bounds:', lbound(x), ubound(x)

        select type (x)
            type is (child)
                do i = lbound(x,1), ubound(x,1)
                    write (*, '(i5,a,a)', advance='no') x(i)%id, ' ', x(i)%name
                end do
                print *, ''
            class default
                error stop 2_4
        end select
    end subroutine
end module

program falloc006a9
use m
    class (*), allocatable :: x1(:)

    allocate (x1(0:2), source=(/child(1, 'x1 01'), child(2, 'x1 02'), &
                        child(3, 'x1 03')/))

    call printX (x1)
end
