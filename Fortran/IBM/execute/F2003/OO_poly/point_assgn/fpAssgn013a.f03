! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/31/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (module data used as
!*                               targets)
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
        procedure, non_overridable :: addID => addID2Base
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

    subroutine addID2Base (b, i)
        class (base), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = b%id + i
    end subroutine

    subroutine addStr2Name (c, ch)
        class (child), intent(inout) :: c
        character(*), intent(in) :: ch

        c%name = trim(c%name)//ch
    end subroutine
end module

module data1
use m
    type (child), save, target :: c1_m
    type (base), save, target :: b1_m = base (id = 10)
end module

program fpAssgn013a
use data1

    class (base), pointer :: b

    call intializeC1_m

    b => c1_m

    call b%addID (9)
    call c1_m%catName ('01')

    b => b1_m

    call b%addID (10)

    call printData
end


subroutine intializeC1_m
use data1
    c1_m = child (1, name = 'c1_m')
end subroutine


subroutine printData
use data1
    call c1_m%print

    call b1_m%print
end subroutine
