! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (use of keyword for
!*                               argument; basic test)
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
    type base
        integer*4 :: id = 1
    end type

    type, extends (base) :: child
        character*20 :: name = 'no-name'
    end type

    contains

    elemental logical function baseEqual (b1, b2)
        class (base), intent(in) :: b1, b2

        baseEqual = (b1%id == b2%id)
    end function
end module

program fArg001
use m
    type (base) :: b1 = base (10)

    type (child) :: c1

    if (baseEqual (b2=base(), b1=b1)) error stop 1_4

    if (.not. baseEqual (b1=child(), b2=base(1))) error stop 2_4

    if (.not. baseEqual (base(10), b2 = child (id=10))) error stop 3_4

    if (.not. baseEqual (c1, b2=base())) error stop 4_4

    if (.not. baseEqual (b2 = c1, b1 = base (1))) error stop 5_4
end