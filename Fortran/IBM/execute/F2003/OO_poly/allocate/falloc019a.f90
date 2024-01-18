! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (the target allocated via allocation
!                               of a pointer has the TARGET attribute; so are
!                               the sub-objects of it)
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
    type seq1
        sequence
        integer(4) :: i1, i2
        integer(8) :: i3
    end type

    type base
        type (seq1) :: data(2) = seq1 (1, 10, i3 = 100_8)
    end type
end module

program falloc019a
use m
    class (base), pointer :: b1_ptr

    class (*), pointer :: x, x1(:)
    type (seq1), pointer :: s, s1(:)

    allocate (b1_ptr)

    x => b1_ptr%data(1)

    x1 => b1_ptr%data

    s => x
    s1 => x1

    if ((s%i1 /= 1) .or. (s%i2 /= 10) .or. (s%i3 /= 100_8)) error stop 1_4

    if ((s1(1)%i1 /= 1) .or. (s1(1)%i2 /= 10) .or. &
        (s1(1)%i3 /= 100_8)) error stop 2_4

    if ((s1(2)%i1 /= 1) .or. (s1(2)%i2 /= 10) .or. &
        (s1(2)%i3 /= 100_8)) error stop 2_4

end
