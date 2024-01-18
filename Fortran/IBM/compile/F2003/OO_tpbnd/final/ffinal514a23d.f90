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
! %POSTCMD: dcomp ffinal514a23d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
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
    end type

    type, extends(base) :: child
        character*20 :: name = 'no-name'

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine
end module

module m1
use m
    type dataType
        class (base), allocatable :: data
    end type

    interface makeData
        function createData (i, c)
            import dataType
            type (dataType) createData
            integer*4, intent(in) :: i
            character(*), intent(in), optional :: c
        end function
    end interface
end module

program ffinal514a23d
use m1
    print *, makeData (10, 'test')
end

function createData (i, c)
    use m1, only: dataType, child, base
    type (dataType) createData
    integer*4, intent(in) :: i
    character(*), intent(in), optional :: c

    type (child), save :: c_static

    if (present (c)) then
        c_static%id = i
        c_static%name = c
        allocate (createData%data, source=c_static)
    else
        allocate (createData%data)
        createData%data%id = i
    end if
end function
