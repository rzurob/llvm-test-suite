! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (deallocate the
!*                               unlimited poly-pointer as a structure component)
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
    end type
end module

module m1
use m
    type, extends (base) :: child
        integer*4, pointer :: value => null()

        contains

        final :: finalizeChild
    end type

    type dataType
        class (*), pointer :: data => null()

        contains

        final :: finalizedataType
    end type

    contains

    subroutine finalizedataType (d)
        type (dataType), intent(inout) :: d

        print *, 'finalizedataType'

        if (associated (d%data)) then
            print *, 'deallocating d%data'
            deallocate (d%data)
        end if

    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c

        print *, 'finalizeChild'

        if (associated (c%value)) then
            print *, 'deallocating c%value'
            deallocate (c%value)
        end if
    end subroutine
end module

program fpAssgn023a
use m1
    type (child), pointer :: c1
    type (dataType), pointer :: d1

    integer*4, pointer :: i1

    allocate (c1, i1, d1)

    i1 = 10

    c1%id = 1
    c1%value => i1

    d1%data => c1

    deallocate (d1)    !<-- this call deallocates c1 and i1 too
end
