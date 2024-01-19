! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly-pointer array
!*                               assigned to an array section of parent
!*                               components of extended target array)
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
    end type

    type, extends(base) :: child
        character*20 :: name

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
end module

program fpAssgn029
use m
    class (base), pointer :: b_ptr (:)

    type(child), target :: c1 (2:10)

    c1 = (/(child(i, 'c1'), i=2,10)/)

    b_ptr => c1(3::2)%base

    do i = 1, 4
        call b_ptr(i)%print
    end do

end
