!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/03/2006
!*
!*  DESCRIPTION                : dtparam
!                               Case: Type parameters used in the prefix for
!                               function declarations.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A(k)
        integer, kind :: k

        type (A(8)), pointer :: p1 => null()
        integer(k) :: id
    end type

    interface A
        type (A(8)) function f1(id)
        import A
            integer(8), intent(in) :: id
        end function

        type (A(4)) function f2(id, p1)
        import A
            integer(4), intent(in) :: id
            type (A(8)), target :: p1
        end function
    end interface
end module

type (A(8)) function f1(id)
use m, only : A
    integer(8), intent(in) :: id

    f1%id = id
end function

type (A(4)) function f2(id, p1)
use m, only: A
    integer(4), intent(in) :: id
    type (A(8)), target :: p1

    f2%id = id
    f2%p1 => p1
end function

program dtparamFuncRet001
use m
    type(A(8)), pointer :: a8_1

    type (A(4)), allocatable :: a4_1(:)

    allocate(a8_1, a4_1(2))

    a8_1 = A(2_8**32+1)

    if (associated(a8_1%p1)) error stop 1_4

    allocate(a8_1%p1)

    a8_1%p1 = A(-10_8)

    if (associated(a8_1%p1%p1)) error stop 2_4

    a4_1(1) = A(100, a8_1)
    a4_1(2) = A(-100, a8_1%p1)

    if (a4_1(1)%id /= 100) error stop 3_4
    if (a4_1(1)%p1%id /= 4294967297_8) error stop 4_4

    if (a4_1(2)%id /= -100) error stop 5_4
    if (a4_1(2)%p1%id /= -10) error stop 6_4
end
