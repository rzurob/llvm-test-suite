! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : IMPLICIT (implicit used in internal subroutine
!*                               call)
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
    implicit type (base) (b), type(child) (c)

    type base
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    target c1_m, b1_m

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fimplct005a1
use m
    implicit class (base) (b)

    pointer b1

    b1_m%id = 1

    b1 => c1_m
    c1_m%id = 10
    c1_m%name = 'c1_m'

    call printd (b1)

    call printd (c1_m)

    call printd (b1_m)

    b1 => b1_m

    call printd (b1)

    contains

    subroutine printd (d)
        implicit class (base) (d)
        intent(in) d

        call d%print
    end subroutine
end

