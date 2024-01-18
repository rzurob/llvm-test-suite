!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal503a.f
! %VERIFY: ffinal503a.out:ffinal503a.vf
! %STDIN:
! %STDOUT: ffinal503a.out
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
!*  DESCRIPTION                : final (module entities are not finalized when
!*                               executing END PROGRAM stmt; use m1 in
!*                               subroutine)
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
        integer*4, pointer :: data(:)

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase(b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        if (associated (b%data)) deallocate (b%data)
    end subroutine
end module

module m1
use m
    type (base) b1_m
    type (base), allocatable :: b2_m
end module

program ffinal503a
    call abc

    print *, 'end'
end

subroutine abc
use m1
    allocate (b1_m%data(2))

    allocate(b2_m)
end subroutine
