!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal514a23.f
! %VERIFY: ffinal514a23.out:ffinal514a23.vf
! %STDIN:
! %STDOUT: ffinal514a23.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 06/24/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : final sub (allocatable component
!                               autodeallocated in intrinsic assignment)
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
        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name = 'no-name'

        contains

        final :: finalizeChild
        procedure :: print => printChild
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

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

module m1
use m
    type dataType
        class (base), allocatable :: data

        contains

        procedure :: print => printData
    end type

    interface makeData
        function createData (i, c)
            import dataType
            type (dataType) createData
            integer*4, intent(in) :: i
            character(*), intent(in), optional :: c
        end function
    end interface

    contains

    subroutine printData (d)
        class (dataType), intent(in) :: d

        if (allocated (d%data)) call d%data%print
    end subroutine
end module

program ffinal514a23
use m1
    type (dataType) :: d1

    d1 = makeData (10, 'test')

    call d1%print

    ! assignment again
    d1 = dataType (data = child())

    call d1%print
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
