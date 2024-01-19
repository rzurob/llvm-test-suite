! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (PASS binding referenced by
!*                               array elements)
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

        procedure, pass :: print => printBase
    end type

    type, extends(base) :: child
        integer*4 :: id

        contains

        procedure, pass :: print => printChild
    end type

    class (base), pointer :: b_ptr

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b
        print *, 'base'
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b
        print *, 'child', b%id
    end subroutine
end module

program ftpbnd502
use m

    type(child), allocatable, target :: c1(:)

    allocate (c1(10))

    c1 = (/(child(10*i), i=2, 11)/)

    do i=1,10
        b_ptr => c1(i)

        call c1(i)%print
        call b_ptr%print
    end do

end
