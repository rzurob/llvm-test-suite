!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp ffinal514a18d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (derived types with allocatable or
!                               pointer component not allowed in IO, only DTIO)
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
    end type

    type dataType
        class (base), pointer :: data => null()

        contains

        final :: finalizeData
    end type

    type (dataType), save :: d1_m

    contains

    subroutine finalizeData (d)
        type (dataType), intent(inout) :: d

        print *, 'finalizeData'

        if (associated (d%data)) then
            print *, 'deallocating d%data'
            deallocate (d%data)
        end if
    end subroutine
end module

module m1
use m, only : base
    type, extends (base) :: child
        integer*4, pointer :: value => null()

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c

        print *, 'finalizeChild'

        if (associated (c%value)) then
            print *, 'deallocating c%value'
            deallocate (c%value)
        end if
    end subroutine
end module

module m2
    interface makeData
        function makeDataOfBase (id)
        use m
            type (dataType) makeDataOFBase
            integer*4, intent(in) :: id
        end function

        function makeDataOfChild (id, value)
        use m
        use m1, only : child
            type (dataType) makeDataOfChild
            integer*4, intent(in) :: id, value
        end function
    end interface
end module

program ffinal514a18d
use m
use m1, only: child
use m2

    print *, makeData(10)
    print *, makeData (10, 10)
end

