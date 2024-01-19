! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (VALUE attribute; the
!*                               changes done on dummy argument do not affect
!*                               the actual argument)
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
        integer*4 :: id
    end type

    type, extends (base) :: child
        character*20 :: name
    end type

    contains

    subroutine increaseTemp (b)
        type (base), value :: b

        b%id = b%id + 10

        print *, b%id
    end subroutine
end module

use m
    type (child) :: c1

    c1 = child (1, "c1")

    call increaseTemp (c1%base)

    print *, c1
end
