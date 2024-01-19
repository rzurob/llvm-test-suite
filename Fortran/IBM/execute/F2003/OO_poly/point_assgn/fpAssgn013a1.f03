! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/31/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (module data used as
!*                               targets; module data are pointer and
!*                               allocatables)
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
        integer*4 :: id = 0

        contains

        procedure :: print => printBase
        procedure, non_overridable :: setID => setBaseID
    end type

    type, extends(base) :: child
        character*20 :: name = ''

        contains

        procedure :: print => printChild
        procedure, non_overridable :: catName => addStr2Name
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

    !! this procedure uses INTENT(OUT) with consequences of reseting the input
    !! data's values as there are default initializations for all components
    subroutine setBaseID (b, i)
        class (base), intent(out) :: b
        integer*4, intent(in) :: i

        b%id = i
    end subroutine

    subroutine addStr2Name (c, ch)
        class (child), intent(inout) :: c
        character(*), intent(in) :: ch

        c%name = trim(c%name)//ch
    end subroutine
end module

module data1
use m
    type (child), target, allocatable :: c1_m
    type (base), pointer :: b1_m => null()
end module

program fpAssgn013a1
use data1

    class (base), pointer :: b

    call intializeModuleData

    b => c1_m

    call b%setID (10)
    call c1_m%catName ('01')

    b => b1_m

    call b%setID (20)

    call printData

    deallocate (b1_m)
end


subroutine intializeModuleData
use data1
    allocate (b1_m, c1_m)

    c1_m = child (1, name = 'c1_m')

    b1_m%id = 10
end subroutine


subroutine printData
use data1
    call c1_m%print

    call b1_m%print
end subroutine
