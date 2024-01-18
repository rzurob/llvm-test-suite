! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (non-poly-pointer
!*                               assigned to type-bound function which returns
!*                               poly pointer)
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
        character*20 :: name

        contains

        procedure :: replicate => replicateBase
        procedure :: print => printBase
    end type

    type, extends(base) :: child
        integer*4 :: id

        contains

        procedure :: replicate => replicateChild
        procedure :: print => printChild
    end type

    type (child) :: c1_m = child ('c1_m', 10)

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%name
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%name, b%id
    end subroutine

    function replicateBase (b)
        class (base), intent(in) :: b
        class (base), pointer :: replicateBase

        allocate (replicateBase)

        replicateBase%name = b%name
    end function

    function replicateChild (b)
        class (base), pointer :: replicateChild
        class (child), intent(in) :: b

        type (child), pointer :: tmp

        allocate (tmp)

        tmp%name = b%name
        tmp%id = b%id

        replicateChild => tmp
    end function
end module

program fpAssgn024a4
use m
    class (base), pointer :: b1
    type (base), pointer :: b2

    type (child) :: c1

    c1 = child ('c1', 20)

    b1 => c1%replicate()

    call b1%print

    deallocate (b1)

    b2 => c1_m%replicate()

    call b2%print
end
