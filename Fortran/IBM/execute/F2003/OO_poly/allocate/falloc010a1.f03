! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (variables with unallocated
!                               allocatable component in source-expr)
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
        character(17) :: name

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        class (*), allocatable :: data(:)

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%name
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        if (allocated (b%data)) then
            print *, b%name, 'size of data :', size(b%data)
        else
            print *, b%name, 'data is not allocated'
        end if
    end subroutine
end module

program falloc010a1
use m
    type (child) :: c1 = child('xlftest', null())
    class (base), pointer :: b1
    class (child), allocatable :: c2

    allocate(b1, source=c1)
    allocate (c2, source=c1)

    call b1%print
    call c2%print
end