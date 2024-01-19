! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/31/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (module array data used
!*                               as TARGETS; test bounds and type-bound)
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
    type (child), save, target :: c1_m(2:11)
    type (base), save, target :: b1_m (-1:3)
end module

program fpAssgn013a2
use data1

    class (base), pointer :: b(:)

    call intializeModuleData

    call printData

    b => c1_m

    if ((lbound(b,1) /= 2) .or. (ubound(b,1) /= 11)) error stop 1_4


    do i = 2, 11
        call b(i)%addID (10)
        call c1_m(i)%catName ('_d')
    end do

    b => b1_m

    if ((lbound(b,1) /= -1) .or. (ubound(b,1) /= 3)) error stop 2_4

    do i = 1, 5
        call b(i-2)%addID (10)
    end do

    call printData
end


subroutine intializeModuleData
use data1
    do i = lbound(c1_m, 1), ubound(c1_m, 1)
        call c1_m(i)%addID (i-1)
        call c1_m(i)%catName ('c1_m_'//char(ichar('0')+i-2))
    end do

    do i = lbound(b1_m, 1), ubound(b1_m, 1)
        call b1_m(i)%addID (i+12)
    end do
end subroutine


subroutine printData
use data1
    do i = lbound(c1_m, 1), ubound(c1_m, 1)
        call c1_m(i)%print
    end do

    do i = lbound(b1_m, 1), ubound(b1_m, 1)
        call b1_m(i)%print
    end do
end subroutine
