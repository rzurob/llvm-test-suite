! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (structure constructor created temps
!*                               finalized; IF-stmt)
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

        final :: finalizeBase
    end type

    type dataType
        class (base), pointer :: data => null()

        contains

        final :: finalizeData
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeData (d)
        type (dataType), intent(inout) :: d

        print *, 'finalizeData'

        if (associated (d%data)) then
            print *, 'deallocating d%data'
            deallocate (d%data)
        end if
    end subroutine

    logical function isAssociated (d)
        class (dataType), intent(in) :: d

        isAssociated = associated (d%data)
    end function
end module

module m1
use m
    type, extends (base) :: child
        character*20 :: name

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine
end module

program ffinal515a5_1
use m1
    class (base), pointer :: b1

    allocate (b1, source=child(10, 'b1'))

    if (isAssociated (dataType(b1)))     print *, 'true'

    print *, 'false condition test'

    if (isAssociated (dataType())) error stop 1_4

    print *, 'end'
end
