! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/14/2005
!*
!*  DESCRIPTION                : final sub (finalization of the temps after the
!                               select type construct)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8) id

        contains

        final :: finalizeBase
        procedure, nopass :: makeData => produceBaseAlloc
    end type

    type, extends (base) :: child
        character(10) name

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    class (base) function produceBaseAlloc (b)
        allocatable :: produceBaseAlloc
        class (base), intent(in) :: b

        allocate (produceBaseAlloc, source=b)
    end function
end module

program ffinal514b3
use m
    class (base), pointer :: b1

    nullify (b1)

    select type (x => b1%makeData(b1%makeData(child (1, 'test'))))
        type is (child)
            print *, x
        class default
            error stop 1_4
    end select

    print *, 'end'
end
