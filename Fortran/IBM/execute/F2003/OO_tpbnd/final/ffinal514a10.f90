!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal514a10.f
! %VERIFY: ffinal514a10.out:ffinal514a10.vf
! %STDIN:
! %STDOUT: ffinal514a10.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of the temps created by
!*                               the function result in PRINT statement;
!*                               function return arrays)
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

        final :: finalizeBase, finalizeBaseRank1
    end type

    private finalizeBase, finalizeBaseRank1

    contains

    subroutine finalizeBase (b)
        type (base), intent (in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

module m1
use m
    type, extends (base) :: child
        character*20 :: name

        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    subroutine finalizeChild (c)
        type (child), intent (in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child), intent (in) :: c(:)

        print *, 'finalizeChildRank1'
    end subroutine
end module

program ffinal514a10
use m1

    interface produceObj
        function produceBase (i, n)
        use m
            type (base) produceBase(n)
            integer*4, intent(in) :: i, n
        end function

        function produceChildObj (i, c, n)
        use m1
            type (child) produceChildObj(n)
            integer*4, intent(in) :: i, n
            character(*), intent(in) :: c
        end function
    end interface

    print *, produceObj (10, 2)
    print *, produceObj (10, 'c1', 2)
    print *, 'end'
end

function produceBase (i, n)
use m
    type (base) produceBase(n)
    integer*4, intent(in) :: i, n

    produceBase%id = i
end function

function produceChildObj (i, c, n)
use m1, only : child
    type (child) produceChildObj(n)
    integer*4, intent(in) :: i
    character(*), intent(in) :: c

    produceChildObj%id = i
    produceChildObj%name = c
end function
