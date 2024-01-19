! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/09/2005
!*
!*  DESCRIPTION                : final sub (finalization of structure component
!                               of rank-two array)
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
        integer id

        contains

        final :: finalizeBase, finalizeBaseArray1, finalizeBaseArray2
    end type

    type dataType
        type (base) :: data(2,2)
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeArray1'
    end subroutine

    subroutine finalizeBaseArray2 (b)
        type(base), intent(in) :: b(:,:)

        print *, 'finalizeBaseArray2'
    end subroutine
end module

program ffinal006a
use m
    class (dataType), pointer :: d1, d2(:), d3(:,:)

    allocate (d1, d2(2), d3(2,2))

    print *, 'test 1'

    deallocate (d1)

    print *, 'test 2'

    deallocate (d2)

    print *, 'test 3'

    deallocate (d3)

    print *, 'end'
end
