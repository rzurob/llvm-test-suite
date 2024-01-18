! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : poly function return (allocatables will get
!                               deallocated after usage)
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
        integer(4) :: id = -1

        contains

        final :: finalizeBase
        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

module m1
use m
    type (child), save :: c1_m
    type (base), save :: b1_m

    contains

    function makeData (b)
        class (base), allocatable :: makeData
        class (base), intent(in) :: b

        allocate (makeData, source=b)
    end function

    subroutine printData (d)
        class (base), intent(in) :: d

        call d%print
    end subroutine

    subroutine printData1 (d)
        type (base), intent(in) :: d

        call d%print
    end subroutine
end module

program ffuncRet007
use m1
    c1_m%id = 1
    c1_m%name = 'c1_m'

    call printData (makeData (c1_m))

    call printData1 (makeData (c1_m))

    print *, 'end'
end
