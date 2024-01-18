! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) for
!                               allocatable dummy-arg; autodeallocation)
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

    type, extends(base) :: child
        character*20 :: name
    end type
end module

program fArg506
use m
    class (child), allocatable :: c1

    allocate (c1)

    call abc(c1)

    if (.not. allocated (c1)) error stop 5_4

    call abc (c1)

    if (allocated (c1))  error stop 6_4

    contains

    subroutine abc (c)
        class(child), allocatable, intent(out) :: c
        logical :: firstTime = .true.

        if (allocated(c)) error stop 1_4

        if (firstTime) then
            firstTime = .false.
            allocate (c)
        end if
    end subroutine
end
