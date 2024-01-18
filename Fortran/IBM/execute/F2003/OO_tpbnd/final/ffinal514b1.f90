! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (allocatable function return result
!                               is automatically deallocated after use)
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
        integer(4) :: id

        contains

        procedure, nopass, non_overridable :: makeObj => produceBaseAlloc
        final :: finalizeBase
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        final :: finalizeChild
    end type

    contains

    function produceBaseAlloc (id, name)
        class (base), allocatable :: produceBaseAlloc

        integer(4), intent(in), optional :: id
        character(*), intent(in), optional :: name

        if (.not. present (id)) then
            return
        else
            if (.not. present (name)) then
                allocate (produceBaseAlloc)
                produceBaseAlloc%id = id
            else
                allocate (produceBaseAlloc, source=child(id, name))
            end if
        end if
    end function

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine
end module

program ffinal514b1
use m
    class (base), allocatable :: b1

    print *, allocated (b1%makeObj())

    if (allocated (b1%makeObj (name='null'))) error stop 1_4

    print *, allocated (b1%makeObj (10))

    if (.not. allocated (b1%makeObj (-1, 'temp'))) error stop 2_4

    print *, 'end'
end
