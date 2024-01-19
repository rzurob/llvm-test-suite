! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/26/2005
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type parameter used in the kind type
!                               param for components: procedure pointer
!                               components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (k)
        integer, kind :: k = 4
        procedure(type(A(k))), pointer, nopass :: p1 => null()
        procedure(type(A(8))), pointer, nopass :: q1 => null()
        integer(k) :: id
    end type

end module

use m
    type(A) a1, a2
    type(A(8)) a3, a4

    type(A(8)) f1
    type(A) f2
    external f1, f2

    a1%p1 => f2
    a1%q1 => f1

    a3%p1 => f1
    a3%q1 => a3%p1

    a2 = a1%p1(100)

    a4 = a3%p1(-100)

    !! verify the results
    if (a2%id /= 100) error stop 1_4
    if (a4%id /= -100) error stop 2_4

    if (associated (a2%p1) .or. associated(a2%q1)) error stop 3_4
    if (associated (a4%p1) .or. associated(a4%q1)) error stop 4_4

    if ((.not. associated (a1%p1, f2)) .or. (.not. associated(a1%q1, f1))) &
            error stop 5_4

    if ((.not. associated(a3%p1, a3%q1)) .or. (.not. associated(a3%p1, f1))) &
            error stop 6_4
end

type (A(8)) function f1(id)
use m, only: A
    integer id
    f1%id = id
end function

type (A(4)) function f2(id)
use m, only: A
    integer, intent(in) :: id

    f2%id = id
end function
