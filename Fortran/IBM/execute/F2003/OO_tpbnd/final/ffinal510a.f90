! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (local allocatable introduced by
!*                               IMPLICIT statement in a subprogram)
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
        integer*4, pointer :: data => null()

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        if (associated (b%data)) then
            print *, 'deallocating data'
            deallocate (b%data)
        end if
    end subroutine
end module

program ffinal510a
use m
    call abc

    print *, 'end of program'
end

subroutine abc
use m
    implicit type (base) (b)

    allocatable b2

    allocate (b2)
    allocate (b2%data)

    print *, 'end of abc'
end subroutine
