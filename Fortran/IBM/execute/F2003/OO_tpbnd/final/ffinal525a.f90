!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal525a.f
! %VERIFY: ffinal525a.out:ffinal525a.vf
! %STDIN:
! %STDOUT: ffinal525a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization process; step 2
!*                               finalize the finalizable components before the
!*                               parent component)
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
    type dataType
        logical :: flag

        contains

        final :: finalizeData
    end type

    type base
        integer*4 :: id

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child
        type (dataType) :: data1
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeData (d)
        type (dataType), intent(in) :: d

        print *, 'finalizeDataType'
    end subroutine
end module

program ffinal525a

    call abc

    print *, 'end'
end

subroutine abc
use m
    type (child) :: c1
end subroutine

