!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal502.f
! %VERIFY: ffinal502.out:ffinal502.vf
! %STDIN:
! %STDOUT: ffinal502.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/31/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final (main entities are not finalized by end
!*                               program statement; use stack variables)
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
        integer*4, pointer :: data(:) => null()

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase(b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        if (associated (b%data)) then
            print *, 'deallocating data'
            deallocate (b%data)
        end if
    end subroutine
end module

use m
    type (base) :: b1

    allocate (b1%data(2))
end
