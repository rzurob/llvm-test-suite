! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (allocatable dummy-arg
!*                               allowed to be associated with unallocated
!*                               actual-arg)
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
        integer*4 :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name = 'default'

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

    subroutine printNdestroy (b)
        class (base), allocatable, intent(inout) :: b

        if (allocated(b)) then
            call b%print

            deallocate (b)
        else
            print *, 'there is nothing to do'
        end if
    end subroutine
end module

program fArg006a
use m
    class (base), allocatable :: b1

    call printNdestroy (b1)

    allocate (b1, source = base(10))

    call printNdestroy (b1)


    call printNdestroy (b1)

    allocate (b1, source = child (20, 'b1'))

    call printNdestroy (b1)
end
