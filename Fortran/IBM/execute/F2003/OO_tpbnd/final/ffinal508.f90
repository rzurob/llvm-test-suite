!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal508.f
! %VERIFY: ffinal508.out:ffinal508.vf
! %STDIN:
! %STDOUT: ffinal508.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (linked list finalization may be
!*                               recursive)
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
        class (base), pointer :: data => null()

        contains

        final :: finalizeBase
    end type

    private finalizeBase

    contains

    recursive subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
        if (associated (b%data)) then
            print *, 'deallocating data'
            deallocate (b%data)
            print *, 'data deallocated'
        end if
    end subroutine
end module

program ffinal508
use m
    type (base), pointer :: b1

    allocate(b1)
    allocate(b1%data)
    allocate (b1%data%data)

    deallocate(b1)

    print *, 'end'
end
