! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/29/2005
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type parameter used for the
!                               determination of kind type parameters for other
!                               type parameters.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A(range, lbound, ubound)
        integer, kind :: range
        integer(selected_int_kind(range)), len :: lbound, ubound

        integer ids(lbound:ubound)
    end type
end module

program kindparamInitexpr009
use m
    type(A(1, lbound=-2, ubound=2)) a1  !<-- length type param is of kind 1
    type(A(4, 5000, 8000)) a2(100)      !<-- length type param is of kind 2
    type(A(17, 2_8**33, 2_8**33+202)) a3!<-- length type param is of kind 8

    !! verify the bounds info
    if ((lbound(a1%ids, 1) /= -2) .or. (ubound(a1%ids, 1) /= 2) .or. &
        (size(a1%ids) /= 5)) error stop 1_4

    if ((lbound(a2(1)%ids,1) /= 5000) .or. (ubound(a2(1)%ids,1) /= 8000) .or. &
        (size(a2(1)%ids) /= 3001)) error stop 2_4

    if ((lbound(a3%ids,1,8) /= 8589934592_8) .or. &
        (ubound(a3%ids,1,8) /= 8589934794_8) .or. &
        (size(a3%ids) /= 203)) error stop 3_4
end
