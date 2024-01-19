! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (automatic deallocation of the
!                               allocated allocatable subobjects in executable
!                               construct)
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

        final :: finalizeBase
        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    type container
        class (base), allocatable :: data(:)
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine printData (d)
        type (container), intent(in) :: d

        if (allocated (d%data)) then
            print *, lbound(d%data, 1), ubound(d%data, 1)

            do i = lbound(d%data, 1), ubound(d%data, 1)
                call d%data(i)%print
            end do
        end if
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

program ffinal531a
use m
    type (child) :: c1(3:5)

    c1%id = (/3,4,5/)
    c1%name = (/'c1_3', 'c1_4', 'c1_5'/)

    print *, 'begin'

    call printData (container (c1))

    print *, 'end'
end
