! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/point_assgn/fpAssgn011ab.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (self-assignment for
!*                               poly-pointers; use derived types as TARGETs)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id
    end type

    class (base(4)), target, allocatable :: b1, b2(:)
end module

program fpAssgn011ab
use m

    class (*), pointer :: x, x1(:)

    allocate (b1, b2(10))

    x => b1

    x => x

    if (.not. associated (x, b1)) error stop 1_4

    x1 => b2

    x1 => x1

    if (.not. associated (x1, b2)) error stop 2_4

    if (size(x1) /= 10) error stop 3_4

    x => x1 (10)

    x => x

    if (.not. associated (x, b2(10))) error stop 4_4
end