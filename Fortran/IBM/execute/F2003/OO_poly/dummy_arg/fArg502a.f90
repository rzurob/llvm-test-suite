! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : arg-association (parameter used as actual arg
!*                               for procedures with poly-dummy-arg)
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
        integer*4 :: id = 0

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name = ''

        contains

        procedure :: print => printChild
    end type

    type (child), parameter :: c1_m = child (1, name = 'c1_m')
    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printData (d)
        class (base), intent(in) :: d

        call d%print
    end subroutine
end module

program fArg502a
use m, only : child, printData, c1_m
    type (child), parameter :: c1 = child (id = 10, name = 'c1')

    call printData (c1_m)

    call printData (c1_m%base)

    call printData (c1)

    call printData (c1%base)
end
