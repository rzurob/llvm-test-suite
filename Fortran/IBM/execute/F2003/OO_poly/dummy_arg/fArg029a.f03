! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (function return results
!                               used as the actual-arg)
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

        contains

        procedure :: print => printBase
        procedure :: replicate => replicateBase
    end type

    type, extends(base) :: child
        character*20 :: name ='default'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    class (base) function replicateBase (b)
        class (base), intent(in) :: b
        pointer replicateBase

        allocate (replicateBase, source=b)
    end function


    subroutine printData (b)
        class (base), intent(in) :: b

        call b%print
    end subroutine
end module

program fArg029a
use m
    class (base), allocatable :: b1

    type (child) :: c1 = child (2, 'c1')

    allocate (b1, source=child(3, 'b1'))

    !! the following 2 calls leak memory
    call printData (c1%replicate())

    call printData (b1%replicate())

end