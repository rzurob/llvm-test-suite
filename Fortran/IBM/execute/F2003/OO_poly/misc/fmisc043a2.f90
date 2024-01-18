!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/28/2005
!*
!*  DESCRIPTION                : miscellaneous (defect 312083)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        integer(8) id
        procedure(integer(8)), pointer, nopass :: ptr

        contains

        procedure :: print => printA
        procedure :: update => updateA
    end type

    contains

    subroutine printA (b)
        class(a), intent(in) :: b

        if (associated(b%ptr)) then
            print *, b%ptr(b%id)
        else
            print *, b%id
        end if
    end subroutine

    subroutine updateA (a1, id, p)
        class(a), intent(inout) :: a1
        integer(8), intent(in), optional :: id
        procedure(integer(8)), optional :: p

        if (present(p)) a1%ptr => p
        if (present(id)) a1%id = id
    end subroutine
end module

use m
    type(A) a1, a2, a3

    procedure(integer(8)), pointer :: t
    integer(8) q
    external q

    nullify(t)

    a1 = A(100, ptr=t)

    if (associated(a1%ptr)) error stop 1_4

    call a1%print

    t => q

    a2 = A (200, ptr=q)
    a3 = A(300, ptr=t)

    if (.not. associated (a3%ptr, a2%ptr)) error stop 2_4

    call a2%print
    call a3%print

    call a1%update (p=t)
    call a2%update (10_8)

    call a1%print
    call a2%print
end

integer(8) function q (i)
    integer(8), intent(in) :: i
    q = i + 10_8
end function
