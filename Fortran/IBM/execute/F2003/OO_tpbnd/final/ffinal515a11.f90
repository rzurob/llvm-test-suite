! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (temps created by structure
!*                               constructors in ALLOCATE statement shall be
!*                               finalized after the statement)
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
        integer*4 :: id = -1

        contains

        final :: finalizeBase
        procedure :: print => printBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine
end module

module m1
use m

    type, extends (base) :: child
        character*20 :: name = 'no-name'

        contains

        final :: finalizeChild
        procedure :: print => printChild
    end type

    contains

    subroutine AllocateData (d, source)
        class (base), intent(out), pointer :: d
        class (base), intent(in) :: source

        allocate (d, source=source)
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine
end module

program ffinal515a11
use m1
    class (base), pointer :: data
    type (child) :: c1

    allocate (data, source=child(10, 'data'))

    call data%print

    deallocate (data)

    print *, 'test 2'

    call AllocateData (data, c1)

    call data%print
end
