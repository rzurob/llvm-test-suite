!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/27/2005
!*
!*  DESCRIPTION                : allocate (data allocation in select type
!                               construct)
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
        integer id
    end type

    type, extends (base) :: child
        character(16) :: name
    end type

    contains

    subroutine copy (x1, x2)
        class (*), allocatable, intent(out) :: x1(:)
        class (*), intent(in) :: x2(:)

        select type (x2)
            type is (real)
                allocate (x1(size(x2)), source=int(x2))
            type is (complex)
                allocate (x1(size(x2)), source=int(x2))
            class default
                allocate (x1(size(x2)), source=x2)
        end select
    end subroutine

    subroutine printX (x)
        class (*), allocatable, intent(in) :: x(:)

        if (allocated (x)) then
            print *, 'bounds: ', lbound(x), ubound(x)
            select type (x)
                type is (base)
                    print *, x
                type is (child)
                    print *, x
                type is (integer)
                    print *, x
                class default
                    print *, 'other data type'
            end select
        end if
    end subroutine
end module

program falloc509a
use m
    class (*), allocatable :: x1(:)

    integer i1(10)
    complex, pointer :: cx (:)

    class (base), allocatable :: b1(:)

    !! test real data type
    call copy (x1, (/1.0, 3.0/))

    call printX (x1)

    !! test integer type
    i1 = (/(i, i=1,10)/)

    call copy (x1, i1(8::2))

    call printX (x1)

    !! test the complex type
    allocate (cx(0:1), source=(/(-1.0, 2.0), (3.1, -1.5)/))

    call  copy (x1, cx)

    call printX (x1)

    !! test derived types
    allocate (b1(3), source=(/child(1, 'xlftest1'), child(2, 'xlftest2'), &
                    child(3, 'xlftest3')/))

    call copy (x1, b1)

    call printX (x1)
end
