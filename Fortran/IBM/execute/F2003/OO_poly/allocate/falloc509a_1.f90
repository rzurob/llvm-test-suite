!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/28/2005
!*
!*  DESCRIPTION                : allocate (allocate statement for unlimited
!                               poly-entities; rank one array and use select
!                               type to verify)
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

program falloc509a_1
use m
    class (*), allocatable :: x1(:)

    integer i1(10)
    complex, pointer :: cx (:)

    class (base), allocatable :: b1(:)

    !! test real data type
    allocate (x1(2), source=int((/1.0, 3.0/)))

    call printX (x1)

    !! test integer type
    i1 = (/(i, i=1,10)/)

    deallocate(x1)

    allocate (x1(2), source=i1(8::2))

    call printX (x1)

    !! test the complex type
    allocate (cx(0:1), source=(/(-1.0, 2.0), (3.1, -1.5)/))

    deallocate(x1)

    allocate (x1(2), source=int(cx))

    call printX (x1)

    !! test derived types
    allocate (b1(3), source=(/child(1, 'xlftest1'), child(2, 'xlftest2'), &
                    child(3, 'xlftest3')/))


    deallocate(x1)

    allocate(x1(3), source=b1)

    call printX (x1)
end
