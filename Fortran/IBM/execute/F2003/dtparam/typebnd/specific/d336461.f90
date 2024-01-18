!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/04/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 336461)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, kind :: n

        contains

        procedure, nopass :: p => func1
        procedure :: print => printBase8
    end type

    contains

    function func1 (i1)
        class(base(8)), pointer :: func1

        nullify(func1)
    end function

    subroutine printBase8 (b1)
        class(base(8)), intent(in) :: b1
    end subroutine
end module

module m1
use m
    type, extends(base) :: child

        integer(n) :: id

        contains

        procedure, nopass :: p => func2
        procedure :: print => printChild8
    end type

    contains

    function func2 (i1)
        class(base(8)), pointer :: func2

        allocate (func2, source=child(8)(i1))
    end function

    subroutine printChild8 (b1)
        class(child(8)), intent(in) :: b1

        print *, b1%id
    end subroutine
end module

use m1
    class(base(1)), allocatable :: b1

    class(base(8)), pointer :: b2

    allocate (b1)

    b2 => b1%p(100)

    if (associated(b2)) error stop 1_4

    deallocate(b1)

    allocate (child(1) :: b1)

    b2 => b1%p(100)

    if (.not. associated(b2)) error stop 2_4

    call b2%print

    deallocate (b2)
end
