! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (pointer or allocatable attribute
!*                               can be specified separately)
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

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        integer*4 :: id = 1

        contains

        procedure :: print => printChild
        final :: finalizeChild, finalizeChild1
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, 'empty type, donot reference'
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChild1 (c1)
        type (child), intent(in) :: c1(:)

        print *, 'finalizeChild1'
    end subroutine
end module

program fclass006
use m
    class (base) b1, b2

    pointer b1
    allocatable b2(:)

    allocate (child :: b2(10))

    print *, size(b2)

    allocate (b1, source=b2(1))

    call b2(2)%print
    call b1%print

    deallocate (b2)
    deallocate (b1)
end
