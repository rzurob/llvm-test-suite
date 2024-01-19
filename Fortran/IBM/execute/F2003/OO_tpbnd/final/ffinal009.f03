! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/09/2005
!*
!*  DESCRIPTION                : final sub (finalization of the pointer returned
!                               from a function call)
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
        integer*4 :: id

        contains

        final :: finalizeBaseArray1, finalizeBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        final :: finalizeChildArray1
    end type

    contains

    function abc ()
        type(child), pointer :: abc(:)

        allocate (abc(10))

        abc%id = (/(i, i= 1, 10)/)
        abc%name = 'abc'
    end function

    subroutine finalizeBase (b)
        type(base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type(base), intent(in) :: b(:)

        print *, 'finalizeBaseArray1'
    end subroutine

    subroutine finalizeChildArray1 (c)
        type(child), intent(in) :: c(:)

        print *, 'finalizeChildArray1'
    end subroutine
end module

program ffinal009
use m
    class (base), pointer :: b1(:)

    b1 => abc()

    if (size(b1) /= 10) error stop 1_4

    if (any (b1%id /= (/(j, j=1,10)/))) error stop 2_4

    deallocate (b1)

    print *, 'end'
end
