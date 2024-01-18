!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/02/2005
!*
!*  DESCRIPTION                : CLASS keyword (select type construct and
!                               defined assignment)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), allocatable :: data
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    interface assignment (=)
        elemental subroutine b1AssgnB2 (b1, b2)
        import base
            class (base), intent(inout) :: b1
            class (base), intent(in) :: b2
        end subroutine
    end interface

    contains

    subroutine printBaseArray (b)
        class (base), intent(in) :: b(:)

        select type (b)
            type is (base)
                do i = 1, size(b)
                    if (allocated (b(i)%data)) write (*, '(f10.2)') b(i)%data
                end do
            type is (child)
                do i = 1, size(b)
                    if (allocated (b(i)%data)) then
                        write (*, '(f10.2, a, a)') b(i)%data, '; ', b(i)%name
                    else
                        write (*, '(a)') b(i)%name
                    end if
                end do
            class default
                error stop 10_4
        end select
    end subroutine
end module


elemental subroutine b1AssgnB2 (b1, b2)
use m, only: base, child
    class (base), intent(out) :: b1
    class (base), intent(in) :: b2

    if (.not. same_type_as (b1, b2)) return

    select type (b2)
        type is (base)
            select type (b1)
                type is (base)
                    b1 = b2
            end select
        type is (child)
            select type (b1)
                type is (child)
                    b1 = b2
            end select
    end select
end subroutine

program fclass012a
use m
    class (*), pointer :: x1(:)

    class (base), allocatable :: b1(:), b2(:)

    allocate (child :: x1(2))
    allocate (b1(0:1), source=(/child(1.2_8, 'abc'), child(2.1_8, 'xyz')/))

    allocate (b2(4), source=(/(base(1.2_8*j), j=1, 3), base(null())/))

    !! test assignment for child type
    select type (x1)
        class is (base)
            x1 = b1

            call printBaseArray(x1)
        class default
            error stop 1_4
    end select


    !! test assignment for base type
    deallocate (x1)

    allocate (base:: x1(size(b2)))

    select type (x1)
        class is (base)
            x1 = b2

            call printBaseArray(x1)
    end select
end
