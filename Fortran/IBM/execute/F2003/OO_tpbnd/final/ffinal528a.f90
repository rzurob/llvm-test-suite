!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal528a.f
! %VERIFY: ffinal528a.out:ffinal528a.vf
! %STDIN:
! %STDOUT: ffinal528a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (nonpointer, nonallocatable local
!*                               variables finalized due to RETURN statement)
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

    type, extends (base) :: child
        integer*4, pointer :: data2 => null()

        contains

        final :: finalizeChild
    end type

    type dataType
        class (base), pointer :: data => null()

        contains

        final :: finalizeData
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        if (associated (b%data))  deallocate (b%data)
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c

        print *, 'finalizeChild'

        if (associated (c%data2)) deallocate (c%data2)
    end subroutine

    subroutine finalizeData (d)
        type (dataType), intent(inout) :: d

        print *, 'finalizeData'

        if (associated (d%data)) deallocate (d%data)
    end subroutine
end module

program  ffinal528
    call abc

    call abc
end

subroutine abc
use m
    type (dataType), save :: d1
    type (dataType) :: d2

    if (.not. associated (d1%data)) then
        print *, 'first time'
        allocate (d1%data)
        allocate (child :: d2%data)
    else
        print *, 'has been here before'
        return
    end if
end subroutine

