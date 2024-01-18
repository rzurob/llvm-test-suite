!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn012a.f
! %VERIFY: fpAssgn012a.out:fpAssgn012a.vf
! %STDIN:
! %STDOUT: fpAssgn012a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (defined operator used
!                               in where construct)
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
        integer(4) id

        contains

        procedure :: print => printBase
        procedure, non_overridable :: getID => getBaseID
    end type

    type, extends (base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    private printBase, getBaseID, printChild

    interface operator (>)
        elemental logical function b1GTb2 (b1, b2)
        import base
            class (base), intent(in) :: b1, b2
        end function

        elemental logical function b1GTi (b1, i)
        import base
            class (base), intent(in) :: b1
            integer(4), intent(in) :: i
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

    elemental integer(4) function getBaseID (b)
        class (base), intent(in) :: b

        getBaseID = b%id
    end function
end module

program fpAssgn012a
use m

    class (base), pointer :: b_ptr(:)

    type (child), target :: c1(50)

    c1 = (/(child(i, 'c1'), i=1,50)/)

    b_ptr => c1

    where (b_ptr > 20)
        b_ptr%id = 20 + mod (b_ptr%id, 2)
    end where

    where (b_ptr > base(20))
        b_ptr%id = 40
    end where

    where (b_ptr%getID() == 40)
        b_ptr%id = -1
    end where

    do i = 1, 50
        if (b_ptr(i)%getID() == -1) print *, i
    end do
end

elemental logical function b1GTb2 (b1, b2)
    use m, only:base

    class (base), intent(in) :: b1, b2

    b1GTb2 = (b1%getID() > b2%getID())
end function

elemental logical function b1GTi (b1, i)
use m, only:base

    class (base), intent(in) :: b1
    integer(4), intent(in) :: i

    b1GTi = (b1%getID() > i)
end function
