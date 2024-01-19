! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : class (argument association,
!*                               poly-dummy-arg-array; resolve type-bounds)
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
        contains

        procedure, nopass :: print => printBase
    end type

    type, extends(base) :: child
        integer*4 :: id

        contains

        procedure, nopass :: print => printChild
    end type

    contains

    subroutine printBase
        print *, 'base'
    end subroutine

    subroutine printChild
        print *, 'child'
    end subroutine
end module

program fclass002
use m

    type (base) :: b1(3)
    type (child) :: c1(30)


    call abc (b1)

    call abc (c1)

    contains

    subroutine abc (a)
        class (base), intent(in) :: a(:)

        print *, size(a)
        call a%print
    end subroutine
end
