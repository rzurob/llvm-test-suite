! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : EXTENDS (test that derived type introduced from
!                               other modules can be extended)
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
    type base(k1)
        integer, kind :: k1
    end type

    type (base(4)) empty
end module

module m1
use m
    type t(k2)
        integer, kind :: k2
        real(k2) :: x
    end type
end module

module m2
use m1
    type, extends (base) :: child(k3)
        integer, kind :: k3
        integer(k3) id
    end type
end module

program fext045
use m2
    class (base(4)), allocatable :: b1

    if (.not. same_type_as (b1, empty)) error stop 1_4

    allocate (b1, source=child (4,4)(id=100))

    if (sizeof (b1) /= 4) error stop 2_4

    if (.not. extends_type_of (b1, empty)) error stop 3_4
end