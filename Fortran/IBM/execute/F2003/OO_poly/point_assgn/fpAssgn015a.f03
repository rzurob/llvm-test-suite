! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (check for bounds info
!                               for data pointer object; use two-dimensional
!                               array)
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
    integer*4 :: i1_m (2:5, -1:4)
    class (*), pointer :: x (:,:)

    type base
        integer*4 :: id = 1
    end type

    type, extends (base) :: child
        character*20 :: name = 'default'
    end type

    type (base), dimension (10, 2:3, 2:5) :: b1_m

    target i1_m, b1_m
    save b1_m
end module

program fpAssgn015a
use m
    class (*), pointer :: x1 (:,:)

    class (base), target, allocatable :: b1 (:, :)

    allocate (child :: b1 (0:4, 2:3))

    !! test assignment to i1_m
    x1 => i1_m

    if ((lbound (x1, 1) /= 2) .or. (lbound (x1, 2) /= -1)) error stop 1_4

    if (any (ubound (x1) /=  (/5, 4/))) error stop 2_4

    if ((size (x1) /= 24) .or. (any (shape (x1) /= (/4,6/)))) error stop 3_4

    if (.not. associated (x1, i1_m)) error stop 4_4

    !! test the assignment to b1

    x => b1

    if ((lbound (x,1) /= 0) .or. (lbound (x, 2) /= 2)) error stop 5_4

    if (any (ubound (x) /= (/4, 3/))) error stop 6_4

    if ((size (x) /= 10) .or. .not. associated (x, b1)) error stop 7_4

    if (any (shape (x) /= (/5,2/))) error stop 8_4


    !! test the array sections from b1_m

    x => b1_m (1, :, ::2)

    if (any (lbound (x) /= 1)) error stop 9_4

    if ((any (shape (x) /= 2)) .or. (size (x) /= 4)) error stop 10_4
end