! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of temps created by
!*                               function result in PRINT statement)
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

    private finalizeBase

    contains

    subroutine finalizeBase (b)
        type (base), intent (in) :: b

        print *, 'finalizeBase'
    end subroutine
end module

module m1
use m
    type, extends (base) :: child
        character*20 :: name

        contains

        final :: finalizeChild
    end type

    interface produceObj
        function produceBase (i)
        use m
            type (base) produceBase
            integer*4, intent(in) :: i
        end function

        function produceChildObj (i, c)
        import child
            type (child) produceChildObj
            integer*4, intent(in) :: i
            character(*), intent(in) :: c
        end function
    end interface

    contains

    subroutine finalizeChild (c)
        type (child), intent (in) :: c

        print *, 'finalizeChild'
    end subroutine
end module

program ffinal514a8
use m1

    print *, produceObj (10)
    print *, produceObj (10, 'test1')
    print *, 'end'
end

function produceBase (i)
use m
    type (base) produceBase
    integer*4, intent(in) :: i

    produceBase%id = i
end function

function produceChildObj (i, c)
use m1, only : child
    type (child) produceChildObj
    integer*4, intent(in) :: i
    character(*), intent(in) :: c

    produceChildObj%id = i
    produceChildObj%name = c
end function
