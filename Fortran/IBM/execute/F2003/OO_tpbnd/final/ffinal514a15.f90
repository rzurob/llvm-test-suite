!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal514a15.f
! %VERIFY: ffinal514a15.out:ffinal514a15.vf
! %STDIN:
! %STDOUT: ffinal514a15.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of temps created by
!*                               function calls in CASE construct)
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

        final :: finalizeBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        final :: finalizeChild
    end type

    interface operator (==)
        logical function baseEqual (b1, b2)
        import base
            type (base), intent(in) :: b1, b2
        end function

        logical function childEqual (c1, c2)
        import child
            type (child), intent(in) :: c1, c2
        end function
    end interface

    interface makeData
        function makeBaseObj (i)
        import base
            type (base) :: makeBaseObj
            integer*4, intent(in) :: i
        end function

        function makeChildObj (i, c)
        import child
            type (child) :: makeChildObj
            integer*4, intent(in) :: i
            character(*), intent(in) :: c
        end function
    end interface

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

logical function baseEqual (b1, b2)
use m, only: base
    type (base), intent(in) :: b1, b2

    baseEqual = (b1%id == b2%id)
end function

logical function childEqual (c1, c2)
use m, only: child, operator(==), base
    type (child), intent(in) :: c1, c2

    childEqual = ((c1%base == c2%base) .and. (c1%name == c2%name))
end function

function makeBaseObj (i)
use m, only: base
    type (base) :: makeBaseObj
    integer*4, intent(in) :: i

    makeBaseObj%id = i
end function

function makeChildObj (i, c)
use m, only:child
    type (child) :: makeChildObj
    integer*4, intent(in) :: i
    character(*), intent(in) :: c

    makeChildObj%id = i
    makeChildObj%name = c
end function


program ffinal514a15
use m
    type (child) :: c1 = child (10, 'c1_static')
    type (base) :: b1 = base (100)

    select case (c1 == makeData (10, 'temp'))
        case (.true.)
            error stop 1_4
        case (.false.)
            print *, 'success'
    end select

    select case (b1 == makeData (100))
        case (.true.)
            print *, 'success'
        case (.false.)
            error stop 2_4
    end select

    print *, (base(10) == makeData (10))
    print *, 'end'
end
