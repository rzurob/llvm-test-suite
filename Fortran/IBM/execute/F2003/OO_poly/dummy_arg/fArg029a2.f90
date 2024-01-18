!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg029a2.f
! %VERIFY: fArg029a2.out:fArg029a2.vf
! %STDIN:
! %STDOUT: fArg029a2.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (function return results
!                               as actual-arg)
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
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name ='default'

        contains

        procedure :: print => printChild
    end type

    interface makeData
        function makeBase (id)
        import base
            integer*4, intent(in) :: id
            type (base) :: makeBase
        end function

        function makeChildData (id, name)
        import child
            integer*4, intent(in) :: id
            character(*), intent(in) :: name

            type (child) :: makeChildData
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printData (b)
        class (base), intent(in) :: b

        call b%print
    end subroutine
end module

program fArg029a2
use m
    call printData (makeData (10))

    call printData (makeData (20, 'temps'))
end

type (base) function makeBase (id)
use m, only: base
    integer*4, intent(in) :: id

    makeBase%id = id
end function

type (child) function makeChildData (id, name)
use m, only:child
    integer*4, intent(in) :: id
    character(*), intent(in) :: name

    makeChildData%id = id
    makeChildData%name = name
end function
