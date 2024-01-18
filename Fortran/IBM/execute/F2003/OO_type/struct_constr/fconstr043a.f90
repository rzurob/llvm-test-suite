!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr043a.f
! %VERIFY: fconstr043a.out:fconstr043a.vf
! %STDIN:
! %STDOUT: fconstr043a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (non-poly pointer
!*                               component used to assign to compatible types;
!*                               use type bound to verify)
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
        type (base), pointer :: next => null()

        contains

        procedure :: print => printBase
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        type (base), pointer :: iterator
        integer*4 :: totCount

        totCount = 0
        iterator => b%next

        do while (associated (iterator))
            totCount = totCount + 1

            iterator => iterator%next
        end do

        if (totCount == 0) then
            print *, 'there is no data in this list'
        else
            print *, totCount, 'node(s) in the list'
        end if
    end subroutine
end module

module m1
use m
    type, extends(base) :: child
        integer*4 :: id

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (b)
        class (child), intent(in) :: b

        call b%base%print

        print *, 'id = ', b%id
    end subroutine
end module

program fconstr043a
use m1
    class (base), pointer :: b_ptr

    type (base) :: b1
    type (child), target :: c1

    c1 = child (id = 10)

    b_ptr => c1

    b1 = base (b_ptr)

    call b1%print

    call b1%next%print

    call b_ptr%print

    b1 = base (c1%base)

    call b1%print

    b1 = base (c1%next)

    call b1%print
end
