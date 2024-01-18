! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of temps created by
!*                               function calls during where statement; a
!*                               reduced test case from ffinal514a11.f)
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
        integer*4 :: id = 1

        contains

        final :: finalizeBase
    end type

    interface makeData
        function makeBase (i)
        import base
            type(base) :: makeBase
            integer*4, intent(in) :: i
        end function
    end interface

    interface operator (==)
        elemental logical function baseEqual (b1, b2)
        import base
            type (base), intent(in) :: b1, b2
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module

module m1
use m
    type, extends(base) :: child
        character*20 :: name = 'no-name'

        contains

        final :: finalizeChild
    end type

    interface makeData
        function makeChildObj (i, c)
        import child
            type (child) makeChildObj
            integer*4, intent(in) :: i
            character(*), intent(in) :: c
        end function
    end interface

    interface operator (.eq.)
        elemental logical function childEqual (c1, c2)
        import child
            type (child), intent(in) :: c1, c2
        end function
    end interface

    contains

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine
end module

program ffinal514a11_2
use m1
    type (child), save :: c1(2:6)

    c1%id = (/(i*10, i=2,6)/)

    c1%name = 'c1_static'

    print *, (c1 == makeData (30, 'c1_static'))

    print *, 'end'
end

function makeBase (i)
use m, only: base
    type(base) :: makeBase
    intent(in) i

    makeBase%id = i
end function

function makeChildObj (i, c)
use m1, only: child
    type (child) makeChildObj
    integer*4, intent(in) :: i
    character(*), intent(in) :: c

    makeChildObj%id = i
    makeChildObj%name = c
end function

elemental logical function baseEqual (b1, b2)
use m, only:base
    type (base), intent(in) :: b1, b2

    baseEqual = (b1%id == b2%id)
end function

elemental logical function childEqual (c1, c2)
use m1, only: base, child, operator(==)
    type (child), intent(in) :: c1, c2

    childEqual = ((c1%base == c2%base) .and. (c1%name == c2%name))
end function
