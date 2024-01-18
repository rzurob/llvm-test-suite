!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/06/2005
!*
!*  DESCRIPTION                : allocate (allocation of zero-size array with
!                               source-expr)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program falloc513
    integer(8), allocatable :: i1(:)
    class(*), allocatable :: x1(:)

    integer lb, ub

    lb =  100
    ub = -100
    allocate (i1(4:1), source=100_8)

    allocate (x1(lb:ub), source= (1.3_4, 2.5_4))

    if (size(i1) /= 0) error stop 1_4

    if ((lbound(i1,1) /= 1) .or. (ubound(i1,1) /= 0)) error stop 2_4

    select type (x1)
        type is (complex(4))
            if (size(x1) /= 0) error stop 3_4

            if ((lbound(x1, 1) /= 1) .or. (ubound(x1,1) /= 0)) error stop 4_4
        class default
            error stop 5_4
    end select
end
