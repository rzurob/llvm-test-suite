! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (SAVE attribute on allocatable
!                               variables)
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

        final :: finalizeBase, finalizeBaseRank1
    end type

    type, extends (base) :: child
        character(20) :: name

        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent (in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b(10:)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child), intent(in) :: c(*)

        print *, 'finalizeChildRank1'
    end subroutine
end module


program ffinal534
    call test1

    print *, 'second call to test1'

    call test1

    print *, 'calling test2'

    call test2

    print *, 'end'
end

subroutine test1
use m
    class (base), allocatable, save :: b1, b2(:), b3 (:,:)

    if (allocated (b1)) then
        print *, 'b1 allocated'
    else
        allocate (child :: b1)
    end if

    if (allocated (b2)) then
        print *, 'b2 allocated, size = ', size (b2)
    else
        allocate (child :: b2(2))
    end if

    if (allocated (b3)) then
        print *, 'b3 allocated, shape = ', shape (b3)
    else
        allocate (b3(2,2))
    end if
end subroutine

subroutine test2
use m
    class (base), allocatable :: b1, b2(:), b3 (:,:)

    allocate (child::b2(2), b1)

    allocate (b3(2,2))
end subroutine