! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (data-pointer assignment
!*                               takes place for pointer component; use
!*                               unlimted poly target for BIND(C) pointer type)
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
    type, bind(C) :: bType
        integer*4 :: i1
    end type

    type base
        type (bType), pointer :: b1
    end type

    class (*), allocatable, target :: x
end module

program fconstr500a
use m
    type (bType), target, allocatable :: bt1
    type (base) :: b1

    allocate (bt1)
    bt1%i1 = 10

    b1 = base (b1 = bt1)

    if (.not. associated (b1%b1, bt1)) error stop 1_4

    allocate (x, source=bt1)

    b1 = base (b1 = x)

    if (.not. associated (b1%b1, x)) error stop 2_4

    if (b1%b1%i1 /= 10) error stop 3_4
end
