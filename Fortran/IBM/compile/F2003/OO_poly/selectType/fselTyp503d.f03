! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/17/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type construct (typo should be
!                               diagnosed)
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

    end type

    type, extends(base) :: child
        integer(4) id
    end type

    contains

    class (base) function f (b)
        class (base), intent(in) :: b

        allocatable f

        allocate (f, source=b)
    end function
end module

program fselTyp503d
use m
    class (base), allocatable :: b

    allocate (child::b)

    select type (x => f1(b))        !<-- typo
        type is (base)
        type is (child)
    end select
end